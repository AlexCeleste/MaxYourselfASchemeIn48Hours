
' Max Yourself a Scheme in 48 Hours
' a translation/reworking from Jonathan Tang's Haskell original
' translated by Alex G ("Yasha")
' licence is CC BY-SA 3.0

' This is the code to the "finished" Scheme interpreter
' If you haven't read through the exercises, go back and look at them first!

SuperStrict

Import "TMeta.bmx"
Import "Functional.bmx"

' data LispVal = Atom String
'              | List [LispVal]
'              | DottedList [LispVal] LispVal
'              | Number Integer
'              | String String
'              | Bool Bool
Type LispVal
End Type

Type LispAtom Extends LispVal
	Field name:String
End Type
Type LispPair Extends LispVal
	Field car:LispVal, cdr:LispVal
End Type
Type LispNum Extends LispVal
	Field val:Double
End Type
Type LispString Extends LispVal
	Field val:String
End Type
Type LispBool Extends LispVal
	Field val:Int
End Type
Type LispChar Extends LispVal
	Field val:Int
End Type
Type LispVector Extends LispVal
	Field elems:LispVal[]
End Type
Type LispPrimitive Extends LispVal
End Type
Type LispFunction Extends LispVal
End Type
Type LispPort Extends LispVal
End Type


Type SchemeLexer	'The interpreter proper
	Function Get:TLexer()
		Function R:TLexRule(r:String, a(l:TLexer), res:String = "", m:String = "")
			Return TLexRule.Create(r, a, res, m)
		End Function
		Global Store(_:TLexer) = TLexAction.Store, Mode(_:TLexer) = TLexAction.Mode, Discard(_:TLexer) = TLexAction.Discard
		
		Const SYM:String = "!#$%&|*+-/:<=>?@^_~~"
		
		Global l:TLexer = TLexer.withRules([..
			R("[0-9]+", Store, "LispNum"),..	'Simple int
			R("#[bBoOdDxX][0-9a-fA-F]+", Store, "LispNum"),..	'Specific-base int, binary/octal/decimal/hex (style: #xABC12)
			R("[0-9]*\.[0-9]+([eE]-?[0-9][0-9]*)?", Store, "LispNum"),..	'Float, simple or scientific
			R("#t|#f", Store, "LispBool"),..	'Boolean
		..
			R("~q([^~q]|\\~q)*~q", Store, "LispString"),..
			R("#\\([\(\)\[\],\.]|([a-zA-Z]+))", Store, "LispChar"),..	'Character constant
		..
			R(";[^\n]*\n", Discard),..			'Line comment: ; B3D-style
		..
			R("\(", Store, "lparen"),..		'Punctuation
			R("\)", Store, "rparen"),..
			R("'",  Store, "quote"),..
			R(",",  Store, "comma"),..
			R("\.", Store, "dot"),..
			R("#",  Store, "hash"),..
		..
			R("[a-z"+SYM+"][a-z0-9"+SYM+"]*", Store, "LispAtom"),..
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
		Expr = "%LispAtom | %LispNum | %LispBool | %LispChar | %LispString | LispList | LispDotted | LispVector | LispQuoted"..
		LispList = "%lparen Expr* %rparen : ~ @elems ~"..
		LispDotted = "%lparen ! Expr+ %dot Expr : ~ @elems ~ @last ~"..
		LispVector = "%hash ! %lparen Expr* %rparen : ~ ~ @elems ~"..
		LispQuoted = "%quote ! Expr : ~ @expr"..
	}
End Type


Try
	Local q:SchemeParser = New SchemeParser
	Local test:String = "(lambda (x y) (* (+ x y) (- x y) 0.3e-4 '(1 2 3) '#(a b c) #t))"
	Print test + "~n"
	Print q.Parse(SchemeLexer.Get().ScanString(test)).ToString()
Catch e:Object
	Print Prettify(e.ToString())
	Function Prettify:String(msg:String)
		Const SRCH:String = "error trying to complete '"
		msg = msg.Replace(SRCH + "(", SRCH + "list")
		msg = msg.Replace(SRCH + "#", SRCH + "vector")
		msg = msg.Replace(SRCH + "(", SRCH + "quoted form")
		Return msg
	End Function
End Try

