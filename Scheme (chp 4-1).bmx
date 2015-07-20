
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
				LispError.Raise "Unrecognised symbol"
				
			Case LispList(val)
				Select True
					Case unCons(unVal("quote"), B).match(val)
						Return LispVal(ConsList(B._).val)
						
					Case unCons(unType(A, LispAtom.Is), B).match(val)
						Return Apply(LispAtom(A._).name, ConsList.Map(ev, ConsList(B._)))
					
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
			LispError.Raise Prettify(e.ToString())
			Function Prettify:String(msg:String)
				Const SRCH:String = "error trying to complete '"
				msg = msg.Replace(SRCH + "(", SRCH + "list").Replace(SRCH + "#", SRCH + "vector")
				msg = msg.Replace(SRCH + "'", SRCH + "quoted form").Replace(SRCH + "`", SRCH + "quasiquoted form")
				Return msg
			End Function
		End Try
	End Function
	
	Function Write:Object(v:LispVal)
		Print v.ToString()
		Return Null
	End Function
End Type


Type SchemeBuiltins
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
		
		Return prims
	End Function
End Type


Local q:SchemeParser = New SchemeParser
Local read:TDelegate = TDelegate.Make(SchemeREPL.Read), eval:TDelegate = TDelegate.Make(SchemeREPL.eval), write:TDelegate = TDelegate.Make(SchemeREPL.Write)
Local _main:TDelegate = write.compose(eval.compose(read.curry(q)))

Try
	_main.call("(< 2 3)")
	_main.call("(> 2 3)")
	_main.call("(>= 3 3)")
	_main.call("(string=? ~qtest~q  ~qtest~q)")
	_main.call("(string>? ~qabc~q  ~qbba~q)")
Catch e:LispError
	Print e.ToString()
End Try

