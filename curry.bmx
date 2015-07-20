
SuperStrict

Type TDelegate
	Const CMAX:Int = 7, ABUF:Int = 2
	Field _cargs:Object[], _ctr:Int
	Field f:Object(_c0:Object, _c1:Object, _c2:Object, _c3:Object, _c4:Object, _c5:Object, _c6:Object, _c7:Object)
	
	Function Make:TDelegate(f:Byte Ptr)
		Local d:TDelegate = New TDelegate
		d.f = f ; d._cargs = New Object[CMAX + ABUF] ; d._ctr = 0
		Return d
	End Function
	
	Method curry:TDelegate(a:Object)
		If _ctr >= CMAX Then RuntimeError "Cannot curry more arguments onto " + ToString() + "; out of room"
		Local r:TDelegate = Make(f)
		r._ctr = _ctr ; r._cargs = New Object[_cargs.Length]
		For Local a:Int = 0 Until _ctr
			r._cargs[a] = _cargs[a]
		Next
		r._cargs[_ctr] = a ; r._ctr :+ 1
		Return r
	End Method
	
	Method call:Object(a:Object)
		_cargs[_ctr] = a
		Return f(_cargs[0], _cargs[1], _cargs[2], _cargs[3], _cargs[4], _cargs[5], _cargs[6], _cargs[7])
	End Method
	Method call2:Object(a0:Object, a1:Object)
		_cargs[_ctr] = a0 ; _cargs[_ctr + 1] = a1
		Return f(_cargs[0], _cargs[1], _cargs[2], _cargs[3], _cargs[4], _cargs[5], _cargs[6], _cargs[7])
	End Method
End Type


Function Cat3:String(s0:String, s1:String, s2:String)
	Print "cat3 " + s0 + " " + s1 + " " + s2
End Function

'Local c:TDelegate = TDelegate.Make(Cat3).curry("Hi!")
'Print String(c.call2("Bye!", "Foo!"))

Type A
End Type

Type b Extends a
End Type

Type c Extends a
End Type

Type d Extends a
End Type

Function _() ; End Function
Local x:a = New b, y:a = New c, z:a = New d
Select x
	Case a(x) ; Print "a"
		Function _() ; End Function
	Case b(x) ; Print "b"
		Function _() ; End Function
	Case c(x) ; Print "c"
End Select

If False Then Print "a" ; Print "b"

Select True
	Case x And y ; Print "xy"
	Case x And d(y) ; Print "xd"
End Select
Print (x And y)
Print (x And d(y))

Select True
	Case fa(), fb() ; Print "A!"
End Select
Function fa:Int() Print "in fa" ; Return 1 End Function
Function fb:Int() Print "in fb" ; Return 1 End Function
