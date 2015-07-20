
' Max Yourself a Scheme in 48 Hours
' a translation/reworking from Jonathan Tang's Haskell original
' translated by Alex G ("Yasha")
' licence is CC BY-SA 3.0

' This is the code to the "finished" Scheme interpreter
' If you haven't read through the exercises, go back and look at them first!

SuperStrict

Import "TMeta.bmx"
Import "Functional.bmx"

Type LispVal
End Type
Type LispAtom Extends LispVal
	Field name:String
	Function Make:LispAtom(n:String)
		Local a:LispAtom = New LispAtom ; a.name = n.ToLower() ; Return a
	End Function
	Method ToString:String() Return name End Method
	Method Compare:Int(with:Object) Return name.Compare(with) End Method
End Type
Type LispList Extends LispVal
	Field vals:ConsList
	Function Make:LispList(vals:Object[], _:Object = Null)
		Local l:LispList = New LispList ; l.vals = ConsList.FromArray(vals) ; Return l
	End Function
	Method ToString:String()
		If vals = Null Then Return "()"
		Local show:TDelegate = TDelegate.Make(_show), join:TDelegate = TDelegate.Make(_join)
		Local l2:ConsList = ConsList.Map(show, vals)
		Return "(" + String(ConsList.FoldL(join, l2.val, l2.nx)) +")"
		Function _show:String(o:Object) Return o.ToString() End Function
		Function _join:String(l:String, r:String) Return l + " " + r End Function
	End Method
End Type
Type LispDottedList Extends LispList
	Field last:LispVal
	Function Make:LispDottedList(vals:Object[], last:Object)
		Local l:LispDottedList = New LispDottedList ; l.vals = ConsList.FromArray(vals) ; l.last = LispVal(last) ; Return l
	End Function
	Method ToString:String()
		Local ret:String = Super.ToString()
		Return ret[..ret.Length - 1] + " . " + last.ToString() + ")"
	End Method
End Type
Type LispNum Extends LispVal
	Field val:Double
	Function Make:LispNum(v:Double)
		Local n:LispNum = New LispNum ; n.val = v ; Return n
	End Function
	Method ToString:String() Return String(val) End Method
End Type
Type LispString Extends LispVal
	Field val:String
	Function Make:LispString(v:String)
		Local s:LispString = New LispString ; s.val = v ; Return s
	End Function
	Method ToString:String() Return "~q" + val.Replace("~n", "\n").Replace("~q", "\~q") + "~q" End Method
End Type
Type LispBool Extends LispVal
	Field val:Int
	Function Make:LispBool(v:Int)
		Local b:LispBool = New LispBool ; b.val = v ; Return b
	End Function
	Method ToString:String()
		If val Then Return "#t" Else Return "#f"
	End Method
End Type
Type LispChar Extends LispVal
	Field val:Int
	Function Make:LispChar(v:Int)
		Local c:LispChar = New LispChar ; c.val = v ; Return c
	End Function
	Method ToString:String()
		If val > 32
			Return "#\" + Chr(val)
		Else
			If val = 32 Return "#\space" ElseIf val = 10 Then Return "#\newline" ElseIf val = 9 Then Return "#\tab"
		EndIf
	End Method	
End Type
Type LispVector Extends LispVal
	Field elems:LispVal[]
	Function Make:LispVector(el:LispVal[])
		Local v:LispVector = New LispVector ; v.elems = el ; Return v
	End Function
	Method ToString:String()
		Local s:String = "#("
		For Local v:LispVal = EachIn elems
			s :+ v.ToString() + " "
		Next
		Return s[..s.Length - 1] + ")"
	End Method
End Type
'Type LispPrimitive Extends LispVal
'	Method ToString:String() ; Return "<builtin>" ; End Method
'End Type
'Type LispFunction Extends LispVal
'End Type
'Type LispPort Extends LispVal
'	Method ToString:String() ; Return "<port>" ; End Method
'End Type


Include "SchemeParser(single).bmx"


Type SchemeREPL
	Function Eval:LispVal(val:LispVal)
		Global ev:TDelegate = TDelegate.Make(Eval)
		Select val
			Case LispAtom(val)
				RuntimeError "Unrecognised symbol"
				
			Case LispList(val)
				Local fst:LispVal = LispVal(LispList(val).vals.val)
				If LispAtom(fst)
					If (LispAtom(fst).name = "quote") Then Return LispVal(LispList(val).vals.nx.val)
					Return Apply(LispAtom(fst).name, ConsList.Map(ev, LispList(val).vals.nx))
				EndIf
				RuntimeError "Incomplete pattern match"
			
			Case LispNum(val), LispString(val), LispBool(val), LispChar(val), LispVector(val)
				Return val
			Default
				Return val'Throw
		End Select
	End Function
	
	Function Apply:LispVal(prim:String, args:ConsList)
		Local op:TDelegate = TDelegate(prims.ValueForKey(prim))
		If op Then Return LispVal(op.call(args)) Else Return LispBool.Make(0)
	End Function
	
	Function Read:LispVal(p:SchemeParser, src:String)
		Try
			Local tree:TParseNode = p.Parse(SchemeLexer.Get().ScanString(src))
			Return p.ToLispVal(tree)
		Catch e:ParseError
			Return LispString.Make(Prettify(e.ToString()))
			Function Prettify:String(msg:String)
				Const SRCH:String = "error trying to complete '"
				msg = msg.Replace(SRCH + "(", SRCH + "list")
				msg = msg.Replace(SRCH + "#", SRCH + "vector")
				msg = msg.Replace(SRCH + "(", SRCH + "quoted form")
				Return msg
			End Function
		End Try
	End Function
	
	Function Write:Object(v:LispVal)
		Print v.ToString()
		Return Null
	End Function
End Type


Function _add:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val + r.val) End Function
Function _sub:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val - r.val) End Function
Function _mul:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val * r.val) End Function
Function _div:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val / r.val) End Function

Function NumBin:LispVal(op:TDelegate, a:ConsList)
	Global isNum:TDelegate = TDelegate.Make(EnsureNum)
	Return LispVal(ConsList.FoldL1(op, ConsList.Map(isNum, a)))

	Function EnsureNum:LispNum(v:LispVal)
		Select v
			Case LispNum(v) ; Return LispNum(v)
			Case LispString(v) ; Return LispNum.Make(Double(LispString(v).val))
			Default ; Return LispNum.Make(0)
		End Select
	End Function
End Function

Local numericBinop:TDelegate = TDelegate.Make(NumBin)', wrap:ProcWrapper(f:Byte Ptr) = ProcWrapper.Make

Global prims:TMap = CreateMap()
prims.Insert "+", numericBinop.curry(TDelegate.Make(_add))
prims.Insert "-", numericBinop.curry(TDelegate.Make(_sub))
prims.Insert "*", numericBinop.curry(TDelegate.Make(_mul))
prims.Insert "/", numericBinop.curry(TDelegate.Make(_div))


Local q:SchemeParser = New SchemeParser
Local read:TDelegate = TDelegate.Make(SchemeREPL.Read), eval:TDelegate = TDelegate.Make(SchemeREPL.eval), write:TDelegate = TDelegate.Make(SchemeREPL.Write)
Local _main:TDelegate = write.compose(eval.compose(read)).curry(q)

_main.call("'bar")
_main.call("(quote (x y))")
_main.call("(+ 2 4)")
_main.call("(- (+ 4 6 3) 3 5 2)")

