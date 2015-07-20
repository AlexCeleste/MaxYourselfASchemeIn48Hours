

Type LispVal
	Function Is:Object(o:Object) Abstract
End Type
Type LispAtom Extends LispVal
	Field name:String
	Function Make:LispAtom(n:String)
		Local a:LispAtom = New LispAtom ; a.name = n.ToLower() ; Return a
	End Function
	Method ToString:String() Return name End Method
	Method Compare:Int(with:Object) Return name.Compare(with) End Method
	Function Is:Object(o:Object) Return LispAtom(o) End Function
End Type
Type LispList Extends LispVal
	Field vals:ConsList
	Global Nil:LispList = LispList.Make(Null)
	Function Make:LispList(vals:LispVal[], _: LispVal = Null)
		Local l:LispList = New LispList ; l.vals = ConsList.FromArray(vals) ; Return l
	End Function
	Function FromCons:LispList(c:ConsList, _:LispVal = Null)
		If c = Null Then Return Nil
		Local l:LispList = New LispList ; l.vals = c ; Return l
	End Function
	Method ToString:String()
		If vals = Null Then Return "()"
		Local show:TDelegate = TDelegate.Make(_show), join:TDelegate = TDelegate.Make(_join)
		Local l2:ConsList = ConsList.Map(show, vals)
		Return "(" + String(ConsList.FoldL(join, l2.val, l2.nx)) +")"
		Function _show:String(o:Object) Return o.ToString() End Function
		Function _join:String(l:String, r:String) Return l + " " + r End Function
	End Method
	Method SendMessage:Object(msg:Object, ctx:Object)
		If msg = RefCell.GetCons Then Return vals Else Return Null
	End Method
	Function Is:Object(o:Object) Return LispList(o) End Function
End Type
Type LispDottedList Extends LispList
	Field last:LispVal
	Function Make:LispList(vals:LispVal[], last:LispVal)
		If LispDottedList(last)
			vals :+ LispVal[](ConsList.ToArray(LispDottedList(last).vals))
			last = LispDottedList(last).last
		ElseIf LispList(last)
			Return LispList.Make(vals + LispVal[](ConsList.ToArray(LispList(last).vals)))
		EndIf
		Local l:LispDottedList = New LispDottedList ; l.vals = ConsList.FromArray(vals) ; l.last = last ; Return l
	End Function
	Function FromCons:LispList(c:ConsList, last:LispVal)
		Local l:LispDottedList = New LispDottedList ; l.vals = c ; l.last = last ; Return l
	End Function
	Method ToString:String()
		Local ret:String = Super.ToString()
		Return ret[..ret.Length - 1] + " . " + last.ToString() + ")"
	End Method
	Function Is:Object(o:Object) Return LispDottedList(o) End Function
End Type
Type LispNum Extends LispVal
	Field val:Double
	Function Make:LispNum(v:Double)
		Local n:LispNum = New LispNum ; n.val = v ; Return n
	End Function
	Method ToString:String()
		If Double(Long(val)) = val Then Return String(Long(val)) Else Return String(val)
	End Method
	Function Is:Object(o:Object) Return LispNum(o) End Function
End Type
Type LispString Extends LispVal
	Field val:String
	Function Make:LispString(v:String)
		Local s:LispString = New LispString ; s.val = v ; Return s
	End Function
	Method ToString:String()
		Return "~q" + (val.Replace("~n", "\n").Replace("~q", "\~q")) + "~q"
	End Method
	Function Is:Object(o:Object) Return LispString(o) End Function
End Type
Type LispBool Extends LispVal
	Field val:Int
	Global _False:LispBool = LispBool.Make(0), _True:LispBool = LispBool.Make(1)
	Function Make:LispBool(v:Int)
		Local b:LispBool = New LispBool ; b.val = (v <> 0) ; Return b
	End Function
	Method ToString:String()
		If val Then Return "#t" Else Return "#f"
	End Method
	Function Is:Object(o:Object) Return LispBool(o) End Function
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
	Function Is:Object(o:Object) Return LispChar(o) End Function	
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
	Function Is:Object(o:Object) Return LispVector(o) End Function
End Type


' From Chapter 6 onward:
Type LispFunc Extends LispVal
	Field args:ConsList, vararg:String, body:ConsList, closure:SchemeEnv
	Function Make:LispFunc(args:ConsList, vararg:String, body:ConsList, closure:SchemeEnv)
		Local f:LispFunc = New LispFunc
		Function _:String(o:Object) Return o.ToString() End Function ; Global toS:TDelegate = TDelegate.Make(_)
		f.args = ConsList.Map(toS, args)
		f.vararg = vararg ; f.body = body ; f.closure = closure
		Return f
	End Function
	Method ToString:String()
		Local s:String = "(lambda ("
		If args Then s :+ ConsList.FoldL1(TDelegate.Make(_), args).ToString()	'Could be a LispVal or a String
		Function _:String(l:LispVal, r:LispVal)
			Return l.ToString() + " " + r.ToString()
		End Function
		If vararg <> "" Then s :+ " . " + vararg
		Return s + ") ...)"
	End Method
	Function Is:Object(o:Object) Return LispFunc(o) End Function
End Type
Type LispPrimitiveFunc Extends LispVal
	Field f:TDelegate
	Function Make:LispPrimitiveFunc(d:TDelegate)
		Local f:LispPrimitiveFunc = New LispPrimitiveFunc ; f.f = d ; Return f
	End Function
	Method ToString:String() Return "<primitive>" End Method
	Function Is:Object(o:Object) Return LispPrimitiveFunc(o) End Function
End Type


' From Chapter 7 onward:
Type LispDeferredTailCall Extends LispVal
	Field env:SchemeEnv, val:LispVal
	Function Make:LispDeferredTailCall(env:SchemeEnv, val:LispVal)
		Local tc:LispDeferredTailCall = New LispDeferredTailCall
		tc.env = env ; tc.val = val ; Return tc
	End Function
	Function Is:Object(o:Object) Return LispDeferredTailCall(o) End Function
End Type


' From Chapter 8 onward:
Type LispPort Extends LispVal
	Field stream:TStream, cached:LispVal[]
	Global StdIn:LispPort = LispPort.FromStream(StandardIOStream), StdOut:LispPort = LispPort.FromStream(StandardIOStream)
	Function Make:LispPort(mode:String, path:String)
		Local p:LispPort = New LispPort
		Select mode
			Case "READMODE" ; p.stream = ReadStream(path)
			Case "WRITEMODE" ; p.stream = WriteStream(path)
		End Select
		If p.stream = Null Then LispError.Raise "Unable to open file '" + path + "'"
		Return p
	End Function
	Function FromStream:LispPort(str:TStream)
		Local p:LispPort = New LispPort
		p.stream = str ; Return p
	End Function
	Method Delete()
		If stream Then stream.Close()
	End Method
	Method ToString:String() Return "<IO port>" End Method
	Function Is:Object(o:Object) Return LispPort(o) End Function
End Type


' From Chapter 10:
Type LispMacro Extends LispFunc
	Function FromFunc:LispMacro(f:LispFunc)
		Local m:LispMacro = New LispMacro
		m.args = f.args ; m.vararg = f.vararg ; m.body = f.body ; m.closure = f.closure
		Return m
	End Function
	Method ToString:String()
		Return "(macro" + Super.ToString()[7..]
	End Method
	Function Is:Object(o:Object) Return LispMacro(o) End Function
End Type

