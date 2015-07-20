
' Max Yourself a Scheme in 48 Hours
' a translation/reworking from Jonathan Tang's Haskell original
' translated by Alex G ("Yasha")
' licence is CC BY-SA 3.0

' This is the code to the "finished" Scheme interpreter
' If you haven't read through the exercises, go back and look at them first!

SuperStrict

Import "TMeta.bmx"
Import "Functional.bmx"

Include "SchemeParser.bmx"
Include "SchemeTypes.bmx"

Type LispError
	Field msg:String
	Function Raise(msg:String)
		Local e:LispError = New LispError ; e.msg = msg ; Throw e
	End Function
	Function ArgCount(expect:Int, got:ConsList)
		Local temp:LispLIst = New LispList ; temp.vals = got	'For printing
		Raise "wrong number of arguments: expected " + expect + ", received actual arguments " + temp.ToString()
	End Function
	Function TypeMismatch(expect:String, got:LispVal)
		Raise "wrong argument type: expected value of type " + expect + ", received actual value " + got.ToString()
	End Function
	Method ToString:String()
		Return "Scheme interpreter error: " + msg
	End Method
End Type


Type SchemeREPL
	Function Eval:LispVal(val:LispVal)
		Global ev:TDelegate = TDelegate.Make(Eval), unCons:RefCell(l:Object, r:RefCell) = RefCell.unCons
		Global unVal:RefCell(_:Object) = RefCell.unVal, unType:RefCell(_:Object, t:Object(_:Object)) = RefCell.unType
		Local A:RefCell = RefCell.Make(), B:RefCell = New A, C:RefCell = New A, D:RefCell = New A
		
		Select val
			Case LispAtom(val)
				LispError.Raise "Unrecognised symbol '" + LispAtom(val).name + "'"
				
			Case LispList(val)
				Select True
					Case unCons(unVal("quote"), B).match(val)
						Return LispVal(ConsList(B._).val)
						
					Case unCons(unVal("if"), unCons(A, unCons(B, unCons(C, Null)))).match(val)
						Local pred:LispVal = Eval(LispVal(A._))
						If LispBool(pred) And (LispBool(pred).val = 0) ..
							Then Return Eval(LispVal(C._)) ..
							Else Return Eval(LispVal(B._))
					Case unCons(unVal("if"), B).match(val) ; LispError.Raise "Eval: malformed 'if' expression: " + val.ToString()
						
					Case unCons(unType(A, LispAtom.Is), B).match(val)
						Return Apply(LispAtom(A._).name, ConsList.Map(ev, ConsList(B._)))
					Case unCons(unType(A, LispAtom.Is), Null).match(val)
						Return Apply(LispAtom(A._).name, Null)
						
					Default
						LispError.Raise "Eval: incomplete pattern match!"
				End Select
				
			Case LispNum(val), LispString(val), LispBool(val), LispChar(val), LispVector(val)
				Return val
			Default
				Return val'Throw
		End Select
	End Function
	
	Function Apply:LispVal(prim:String, args:ConsList)
		Local op:TDelegate = TDelegate(SchemeBuiltins.prims.ValueForKey(prim))
		If op
			Return LispVal(op.call(args))
		Else
			LispError.Raise "Apply: unrecognised function '" + prim + "'"
		EndIf
	End Function
	
	Function Read:LispVal(p:SchemeParser, src:String)
		Try
			Local tree:TParseNode = p.Parse(SchemeLexer.Get().ScanString(src))
			Return p.ToLispVal(tree)
		Catch e:ParseError
			Local msg:String = e.ToString(), SRCH:String = "error trying to complete '"
			msg = msg.Replace(SRCH + "(", SRCH + "list").Replace(SRCH + "#", SRCH + "vector")
			msg = msg.Replace(SRCH + "'", SRCH + "quoted form").Replace(SRCH + "`", SRCH + "quasiquoted form")
			LispError.Raise msg
		End Try
	End Function
	
	Function Write:Object(v:LispVal)
		Print v.ToString()
		Return Null
	End Function
End Type


Type SchemeBuiltins
	Global unCons:RefCell(l:Object, r:RefCell) = RefCell.unCons, ..	'THese are only for the implementation of the actual prims
	       unVal:RefCell(_:Object) = RefCell.unVal, unType:RefCell(_:Object, t:Object(_:Object)) = RefCell.unType
	
	Function _add:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val + r.val) End Function
	Function _sub:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val - r.val) End Function
	Function _mul:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val * r.val) End Function
	Function _div:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val / r.val) End Function
	
	Function _numEq:LispBool(l:LispNum, r:LispNum) Return LispBool.Make(l.val = r.val) End Function
	Function _numLt:LispBool(l:LispNum, r:LispNum) Return LispBool.Make(l.val < r.val) End Function
	Function _numGt:LispBool(l:LispNum, r:LispNum) Return LispBool.Make(l.val > r.val) End Function
	Function _numNe:LispBool(l:LispNum, r:LispNum) Return LispBool.Make(l.val <> r.val) End Function
	Function _numLe:LispBool(l:LispNum, r:LispNum) Return LispBool.Make(l.val <= r.val) End Function
	Function _numGe:LispBool(l:LispNum, r:LispNum) Return LispBool.Make(l.val >= r.val) End Function
	
	Function _boolAnd:LispBool(l:LispBool, r:LispBool) Return LispBool.Make(l.val And r.val) End Function
	Function _boolOr:LispBool(l:LispBool, r:LispBool) Return LispBool.Make(l.val Or r.val) End Function
	
	Function _strEq:LispBool(l:LispString, r:LispString) Return LispBool.Make(l.val = r.val) End Function
	Function _strLt:LispBool(l:LispString, r:LispString) Return LispBool.Make(l.val.Compare(r.val) < 0) End Function
	Function _strGt:LispBool(l:LispString, r:LispString) Return LispBool.Make(l.val.Compare(r.val) > 0) End Function
	Function _strLe:LispBool(l:LispString, r:LispString) Return LispBool.Make(l.val.Compare(r.val) <= 0) End Function
	Function _strGe:LispBool(l:LispString, r:LispString) Return LispBool.Make(l.val.Compare(r.val) >= 0) End Function
	
	Function NumericOp:LispVal(op:TDelegate, a:ConsList)
		Global isNum:TDelegate = TDelegate.Make(UnpackNum)
		Select True
			Case a = Null, a.nx = Null	'Fewer than 2 args
				LispError.ArgCount 2, a
			Default
				Return LispVal(ConsList.FoldL1(op, ConsList.Map(isNum, a)))
		End Select
	End Function
	
	Function BinaryOp:LispVal(unpack:TDelegate, op:TDelegate, args:ConsList)
		If ConsList.Length(args) <> 2 Then LispError.ArgCount 2, args
		Local l:LispVal = LispVal(unpack.call(args.val))
		Local r:LispVal = LispVal(unpack.call(args.nx.val))
		Return LispVal(op.call2(l, r))
	End Function
	
	Function UnpackNum:LispVal(v:LispVal)
		If LispNum(v) Then Return v Else LispError.TypeMismatch "Number", v
	End Function
	Function UnpackStr:LispVal(v:LispVal)
		If LispString(v) Then Return v Else LispError.TypeMismatch "String", v
	End Function
	Function UnpackBool:LispVal(v:LispVal)
		If LispBool(v) Then Return v Else LispError.TypeMismatch "Boolean", v
	End Function
	
	Function Car:LispVal(a:ConsList)
		If ConsList.Length(a) <> 1 Then LispError.ArgCount 1, a
		Local val:LispVal = LispVal(a.val), RET:RefCell = RefCell.Make()
		Select True
			Case unType(unCons(RET, RefCell.Any), LispDottedList.Is).match(val), ..
			     unType(unCons(RET, RefCell.Any), LispList.Is).match(val)
					Return LispVal(RET._)
			Default ; LispError.TypeMismatch "List", val
		End Select
	End Function
	Function Cdr:LispVal(a:ConsList)
		If ConsList.Length(a) <> 1 Then LispError.ArgCount 1, a
		Local val:LispVal = LispVal(a.val), RET:RefCell = RefCell.Make()
		Select True
			Case unType(unCons(RefCell.Any, RET), LispDottedList.Is).match(val)
				Return LispDottedList.FromCons(ConsList(RET._), LispDottedList(val).last)
			Case unType(unCons(RefCell.Any, Null), LispDottedList.Is).match(val)
				Return LispDottedList(val).last
			Case unType(unCons(RefCell.Any, RET), LispList.Is).match(val)
				Return LispList.FromCons(ConsList(RET._))
			Default ; LispError.TypeMismatch "List", val
		End Select
	End Function
	Function Cons:LispVal(args:ConsList)
		If ConsList.Length(args) <> 2 Then LispError.ArgCount 2, args
		Local l:LispVal = LispVal(args.val), r:LispVal = LispVal(args.nx.val)
		Select r
			Case Null
				Return LispList.Make([l])
			Case LispDottedList(r)
				Local dl:LispDottedList = LispDottedList(r)
				Return LispDottedList.FromCons(ConsList.Cons(l, dl.vals), dl.last)
			Case LispList(r)
				Return LispList.FromCons(ConsList.Cons(l, LispList(r).vals))
			Default
				Return LispDottedList.Make([l], r)
		End Select
	End Function
	
	Function EqvP:LispVal(args:ConsList)
		If ConsList.Length(args) <> 2 Then LispError.ArgCount 2, args
		Local l:LispVal = LispVal(args.val), r:LispVal = LispVal(args.nx.val)
		If l = r Then Return LispBool._True
		
		Select True
			Case LispBool(l) And LispBool(r)     ;    Return LispBool.Make(LispBool(l).val = LispBool(r).val)
			Case LispNum(l) And LispNum(r)       ;    Return LispBool.Make(LispNum(l).val = LispNum(r).val)
			Case LispString(l) And LispString(r) ;    Return LispBool.Make(LispString(l).val = LispString(r).val)
			Case LispAtom(l) And LispAtom(r)     ;    Return LispBool.Make(LispAtom(l).name = LispAtom(r).name)
			Case LispList(l) And LispList(r)
				Global eqP:TDelegate = TDelegate.Make(eqvPair), zipEq:TDelegate = TDelegate.Make(ConsList.ZipWith).curry(eqP)
				Function eqvPair:LispVal(l:LispVal, r:LispVal)
					Return EqvP(ConsList.Cons(l, ConsList.Cons(r, Null)))
				End Function
				If ConsList.Length(LispList(l).vals) <> ConsList.Length(LispList(r).vals) Then Return LispBool._False
				Local ret:LispVal = LispVal(ConsList.FoldL(eqP, LispBool._True, ..
				                            ConsList(zipEq.call2(LispList(l).vals, LispList(r).vals))))
				If LispDottedList(l) And LispDottedList(r) Then ..
				   ret = eqvPair(ret, eqvPair(LispDottedList(l).last, LispDottedList(r).last))
				Return ret
				
			Case LispVector(l) And LispVector(r)
				Local lv:LispVector = LispVector(l), rv:LispVector = LispVector(r), ret:Int = 1
				If lv.elems.Length <> rv.elems.Length Then Return LispBool._False
				For Local e:Int = 0 Until lv.elems.Length
					ret = ret & LispBool(EqvP(ConsList.Cons(lv.elems[e], ConsList.Cons(rv.elems[e], Null)))).val
				Next
				Return LispBool.Make(ret)
				
			Default ; Return LispBool._False
		End Select
	End Function
	
	Global prims:TMap = SchemeBuiltins._init()
	Function _init:TMap()
		Local prims:TMap = CreateMap()
		
		Local numericBinop:TDelegate = TDelegate.Make(NumericOp)
		prims.Insert "+", numericBinop.curry(TDelegate.Make(_add))
		prims.Insert "-", numericBinop.curry(TDelegate.Make(_sub))
		prims.Insert "*", numericBinop.curry(TDelegate.Make(_mul))
		prims.Insert "/", numericBinop.curry(TDelegate.Make(_div))
		
		Local binop:TDelegate = TDelegate.Make(SchemeBuiltins.BinaryOp)
		Local numBoolBinop:TDelegate = binop.curry(TDelegate.Make(UnpackNum))
		Local strBoolBinop:TDelegate = binop.curry(TDelegate.Make(UnpackStr))
		Local boolBoolBinop:TDelegate = binop.curry(TDelegate.Make(UnpackBool))
		
		prims.Insert "=", numBoolBinop.curry(TDelegate.Make(_numEq))
		prims.Insert "<", numBoolBinop.curry(TDelegate.Make(_numLt))
		prims.Insert ">", numBoolBinop.curry(TDelegate.Make(_numGt))
		prims.Insert "/=", numBoolBinop.curry(TDelegate.Make(_numNe))
		prims.Insert "<=", numBoolBinop.curry(TDelegate.Make(_numLe))
		prims.Insert ">=", numBoolBinop.curry(TDelegate.Make(_numGe))
		prims.Insert "&&", boolBoolBinop.curry(TDelegate.Make(_boolAnd))
		prims.Insert "||", boolBoolBinop.curry(TDelegate.Make(_boolOr))
		prims.Insert "string=?", strBoolBinop.curry(TDelegate.Make(_strEq))
		prims.Insert "string<?", strBoolBinop.curry(TDelegate.Make(_strLt))
		prims.Insert "string>?", strBoolBinop.curry(TDelegate.Make(_strGt))
		prims.Insert "string<=?", strBoolBinop.curry(TDelegate.Make(_strLe))
		prims.Insert "string>=?", strBoolBinop.curry(TDelegate.Make(_strGe))
		
		prims.Insert "car", TDelegate.Make(Car)
		prims.Insert "cdr", TDelegate.Make(Cdr)
		prims.Insert "cons", TDelegate.Make(Cons)
		
		prims.Insert "eqv?", TDelegate.Make(EqvP)
		prims.Insert "eq?", TDelegate.Make(EqvP)	'eq? and equal? are allowed to be the same as eqv?, so they are
		prims.Insert "equal?", TDelegate.Make(EqvP)
		
		Return prims
	End Function
End Type


Local q:SchemeParser = New SchemeParser
Local read:TDelegate = TDelegate.Make(SchemeREPL.Read), eval:TDelegate = TDelegate.Make(SchemeREPL.eval), write:TDelegate = TDelegate.Make(SchemeREPL.Write)
Local _main:TDelegate = write.compose(eval.compose(read.curry(q)))

Try
	_main.call "(if (> 2 3) 'no 'yes)"
	_main.call "(if (= (+ 1 2) 3) (+ 2 3 (- 5 1)) 'unequal)"
	
	_main.call "(car '(a b . ()))"
	_main.call "(cons 'c (cdr '(a . b)))"
	_main.call "(cons 'c '(a . b))"
	_main.call "(cons 'c '(a  b))"
	_main.call "(cons 'c '())"
	
	_main.call "(eqv? 1 3)"
	_main.call "(eqv? 3 3)"
	_main.call "(eqv? 'atom 'atom)"
	_main.call "(eqv? '(1 2 3) '(1 2 3))"
	_main.call "(eqv? '(1 2 3) '(a d c))"
	_main.call "(eqv? '(a b) '())"
Catch e:LispError
	Print e.ToString()
End Try

