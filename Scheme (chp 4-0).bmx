
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
	
	Function NumBinop:LispVal(op:TDelegate, a:ConsList)
		Global isNum:TDelegate = TDelegate.Make(EnsureNum)
		Select True
			Case a = Null, a.nx = Null	'Fewer than 2 args
				LispError.ArgCount 2, a
			Default
				Return LispVal(ConsList.FoldL1(op, ConsList.Map(isNum, a)))
		End Select
	
		Function EnsureNum:LispVal(v:LispVal)
			Select v
				Case LispNum(v) ; Return v
				Default ; LispError.TypeMismatch "Number", v
			End Select
		End Function
	End Function
	
	Global prims:TMap = SchemeBuiltins._init()
	Function _init:TMap()
		Local prims:TMap = CreateMap()
		Local numericBinop:TDelegate = TDelegate.Make(NumBinop)
		prims.Insert "+", numericBinop.curry(TDelegate.Make(_add))
		prims.Insert "-", numericBinop.curry(TDelegate.Make(_sub))
		prims.Insert "*", numericBinop.curry(TDelegate.Make(_mul))
		prims.Insert "/", numericBinop.curry(TDelegate.Make(_div))
		Return prims
	End Function
End Type


Local q:SchemeParser = New SchemeParser
Local read:TDelegate = TDelegate.Make(SchemeREPL.Read), eval:TDelegate = TDelegate.Make(SchemeREPL.eval), write:TDelegate = TDelegate.Make(SchemeREPL.Write)
Local _main:TDelegate = write.compose(eval.compose(read.curry(q)))

Try
	_main.call("'bar")
	_main.call("(quote (x y))")
	_main.call("(+ 2 4)")
	_main.call("(- (+ 4 6 3) 3 5 2)")
	_main.call("(+ 2 ~q3~q)")
Catch e:LispError
	Print e.ToString()
End Try

