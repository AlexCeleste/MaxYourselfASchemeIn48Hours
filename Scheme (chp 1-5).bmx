
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
	Method ToString:String() ; Return name ; End Method
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
	Method ToString:String() ; Return String(val) ; End Method
End Type
Type LispString Extends LispVal
	Field val:String
	Function Make:LispString(v:String)
		Local s:LispString = New LispString ; s.val = v ; Return s
	End Function
	Method ToString:String() ; Return "~q" + val.Replace("~n", "\n").Replace("~q", "\~q") + "~q" ; End Method
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


Include "SchemeParser.bmx"


Local q:SchemeParser = New SchemeParser
Local test:String = "(lambda (x y) (* (+ x y) (- x #\y) 0.3e-4 '(1 -2 3) '#(a b c) #t))"
Print test + "~n"
Print Read(q, test).ToString()

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
