

Type SchemeLexer
	Function Get:TLexer()
		Function R:TLexRule(r:String, a(l:TLexer), res:String = "", m:String = "")
			Return TLexRule.Create(r, a, res, m)
		End Function
		Global Store(_:TLexer) = TLexAction.Store, Mode(_:TLexer) = TLexAction.Mode, Discard(_:TLexer) = TLexAction.Discard
		
		Const SYM:String = "!$%&|*+-/:<=>?^_~~"
		
		Global l:TLexer = TLexer.withRules([..
			R("(\+|-)?[0-9]+", Store, "LispNum"),..	'Simple int
			R("#[bBoOdDxX][0-9a-fA-F]+", Store, "LispNum"),..	'Specific-base int, binary/octal/decimal/hex (style: #xABC12)
			R("(\+|-)?[0-9]*\.[0-9]+([eE]-?[0-9][0-9]*)?", Store, "LispNum"),..	'Float, simple or scientific
			R("(#t|#f)", Store, "LispBool"),..	'Boolean
		..
			R("~q([^~q]|\\~q)*~q", Store, "LispString"),..
			R("#\\([\(\)\[\],\.'`~q#@"+SYM+"]|([a-zA-Z]+))", Store, "LispChar"),..	'Character constant
		..
			R(";[^\n]*\n", Discard),..			'Line comment: ; B3D-style
		..
			R("\(", Store, "lparen"),..		'Punctuation
			R("\)", Store, "rparen"),..
			R("'",  Store, "quote"),..
			R("`",  Store, "backquote"),..
			R(",",  Store, "comma"),..
			R(",@", Store, "splice"),..
			R("\.", Store, "dot"),..
			R("(#)", Store, "hash"),..
		..
			R("[a-z"+SYM+"][a-z0-9@"+SYM+"]*", Store, "LispAtom"),..
		..
		..	'Obvious lex-time errors:
			R("[^[:space:]]", TLexAction.Error, "unrecognised character"),..		'Any other printable character
			R("[0-9]+[a-z_]", TLexAction.Error, "invalid identifier/number")..
		])
		
		l.SetCaseSensitivity False
		l.SetGuardMode True
		Return l
	End Function
End Type

Type SchemeParser Extends TMetaParser Final
	Field grammar:TMap {..
		Prog = "Expr* : @program"..
		Expr = "%LispAtom | %LispNum | %LispBool | %LispChar | %LispString | List | Dotted | Vector | Quoted | QQuote | UnQuote | Splice"..
		List = "%lparen Expr* %rparen : ~ @elems ~"..
		Dotted = "%lparen ! Expr+ %dot Expr %rparen : ~ @elems ~ @last ~"..
		Vector = "%hash ! %lparen Expr* %rparen : ~ ~ @elems ~"..
		Quoted = "%quote ! Expr : ~ @expr"..
		QQuote = "%backquote ! Expr : ~ @expr"..
		UnQuote = "%comma Expr : ~ @expr"..
		Splice = "%splice Expr : ~ @expr"..
	}
	
	Function ToLispVals:LispVal[](ptree:TParseNode)
		If ptree.elem And (ptree.rule = "" Or ptree.rule = "Prog")
			Local vals:LispVal[] = New LispVal[ptree.elem.Length]
			For Local e:Int = 0 Until vals.Length
				vals[e] = ToLispVal(ptree.elem[e])
			Next
			Return vals
		Else
			Return [ToLispVal(ptree)]
		EndIf
	End Function
	
	Function ToLispVal:LispVal(ptree:TParseNode)
		Select ptree.rule
			Case "List"
				If ptree.elem = Null And ptree.term = Null Then Return LispList.Nil		'()
				Local pEl:TParseNode[] = ptree.GetElem("elems").elem, vals:LispVal[] = New LispVal[pEl.Length]
				If pEl = Null Then Return LispList.Make([ToLispVal(ptree.GetElem("elems"))])	'Single-element
				For Local e:Int = 0 Until pEl.Length
					vals[e] = ToLispVal(pEl[e])
				Next
				Return LispList.Make(vals)
				
			Case "Dotted"
				Local pEl:TParseNode[] = ptree.GetElem("elems").elem, vals:LispVal[] = New LispVal[pEl.Length]
				If pEl = Null
					vals = [ToLispVal(ptree.GetElem("elems"))]
				Else
					For Local e:Int = 0 Until pEl.Length
						vals[e] = ToLispVal(pEl[e])
					Next
				EndIf
				Local last:LispVal = ToLispVal(ptree.GetElem("last"))
				Return LispDottedList.Make(vals, last)
				
			Case "Vector"
				Local pEl:TParseNode[] = ptree.GetElem("elems").elem, el:LispVal[] = New LispVal[pEl.Length]
				For Local e:Int = 0 Until el.Length
					el[e] = ToLispVal(pEl[e])
				Next
				Return LispVector.Make(el)
				
			Case "Quoted" ; Return wrap("quote", ToLispVal(ptree.GetElem("expr")))
			Case "QQuote" ; Return wrap("quasiquote", ToLispVal(ptree.GetElem("expr")))
			Case "UnQuote" ; Return wrap("unquote", ToLispVal(ptree.GetElem("expr")))
			Case "Splice" ; Return wrap("unquote-splicing", ToLispVal(ptree.GetElem("expr")))
		End Select
		Function wrap:LispVal(cmd:String, qval:LispVal) Return LispList.Make([LispVal(LispAtom.Make(cmd)), qval]) End Function
		
		Local term:TToken = ptree.term
		If term = Null Then Return LispBool._False
		Select term.tType
			Case "LispAtom" ; Return LispAtom.Make(term.value)
			Case "LispBool" ; Return LispBool.Make(term.value = "#t")
			Case "LispString"
				Local s:String = term.value
				Return LispString.Make(s[1..s.Length - 1].Replace("\n", "~n").Replace("\~q", "~q"))
			Case "LispChar"
				Local ch:String = term.value[2..]
				If ch.Length = 1 Then Return LispChar.Make(ch[0])
				Select ch
					Case "newline" ; LispChar.Make(10)
					Case "space" ; LispChar.Make(32)
					Case "tab" ; LispChar.Make(9)
					Default LispChar.Make(" "[0])'Throw
				End Select
			Case "LispNum" ; Return LispNum.Make(Double(term.value))
		End Select
	End Function
End Type

