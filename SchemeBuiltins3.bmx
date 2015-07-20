

Type SchemeBuiltins
	Global unCons:RefCell(l:Object, r:RefCell) = RefCell.unCons, unVal:RefCell(_:Object) = RefCell.unVal, ..
	       unType:RefCell(_:Object, t:Object(_:Object)) = RefCell.unType, unMaybe:RefCell(_:RefCell) = RefCell.unMaybe
	
	Function _add:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val + r.val) End Function
	Function _sub:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val - r.val) End Function
	Function _mul:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val * r.val) End Function
	Function _div:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val / r.val) End Function
	Function _mod:LispNum(l:LispNum, r:LispNum) Return LispNum.Make(l.val Mod r.val) End Function
	
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
			Case unType(unCons(RefCell.Any, unMaybe(RET)), LispList.Is).match(val)
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
	
	Function _apply:LispVal(a:ConsList)
		If ConsList.Length(a) <> 2 Then LispError.ArgCount 2, a
		Return SchemeREPL.Apply(LispVal(a.val), a.nx)
	End Function
	Function _makePort:LispVal(s:String, a:ConsList)
		If ConsList.Length(a) <> 1 Then LispError.ArgCount 1, a
		If Not LispString(a.val) Then LispError.TypeMismatch "String", LispVal(a.val)
		Return LispPort.Make(s, LispString(a.val).val)
	End Function
	Function _closePort:LispVal(a:ConsList)
		If ConsList.Length(a) <> 1 Then LispError.ArgCount 1, a
		Local p:LispPort = LispPort(a.val) ; If Not p Then LispError.TypeMismatch "Port", LispVal(a.val)
		If p.stream Then p.stream.Close() ; p.stream = Null
		Return LispList.Nil
	End Function
	Function _read:LispVal(a:ConsList)
		If ConsList.Length(a) > 1 Then LispError.ArgCount 1, a
		Local p:LispPort = LispPort(a.val) ; If a And Not p Then LispError.TypeMismatch "Port", LispVal(a.val)
		Return SchemeREPL.ReadOne(New SchemeParser, p)
	End Function
	Function _write:LispVal(a:ConsList)
		Local ln:Int = ConsList.Length(a)
		If ln > 2 Or ln < 1 Then LispError.ArgCount 2, a
		Local p:LispPort = LispPort.StdOut
		If ln = 2 Then p = LispPort(a.nx.val) ; If a And Not p Then LispError.TypeMismatch "Port", LispVal(a.val)
		Return SchemeREPL.Write(p, LispVal(a.val))
	End Function
	Function _readContents:LispString(a:ConsList)
		If ConsList.Length(a) <> 1 Then LispError.ArgCount 1, a
		Local p:LispPort = LispPort(a.val) ; If Not p Then LispError.TypeMismatch "Port", LispVal(a.val)
		Local s:String ; While Not Eof(p.stream)
			s :+ p.stream.ReadLine() + "~n"
		Wend
		Return LispString.Make(s)
	End Function
	Function _readAll:LispList(a:ConsList)
		Return LispList.Make(SchemeREPL.Read(New SchemeParser, _readContents(a).val))
	End Function
	Function _print:LispVal(a:ConsList)
		If ConsList.Length(a) <> 1 Then LispError.ArgCount 1, a
		Local s:String = a.val.ToString()
		If LispString(a.val) Then Print s[1..s.Length - 1] Else Print s
		Return LispList.Nil
	End Function
	
	Global prims:SchemeEnv = SchemeBuiltins._init()
	Function _init:SchemeEnv()
		Local prims:SchemeEnv = SchemeEnv.Make()
		Function addPrim(env:SchemeEnv, name:String, f:TDelegate)
			SchemeEnv.DefineVar env, name, LispPrimitiveFunc.Make(f)
		End Function
		
		Local numericBinop:TDelegate = TDelegate.Make(NumericOp)
		addPrim prims, "+", numericBinop.curry(TDelegate.Make(_add))
		addPrim prims, "-", numericBinop.curry(TDelegate.Make(_sub))
		addPrim prims, "*", numericBinop.curry(TDelegate.Make(_mul))
		addPrim prims, "/", numericBinop.curry(TDelegate.Make(_div))
		addPrim prims, "mod", numericBinop.curry(TDelegate.Make(_mod))
		
		Local binop:TDelegate = TDelegate.Make(SchemeBuiltins.BinaryOp)
		Local numBoolBinop:TDelegate = binop.curry(TDelegate.Make(UnpackNum))
		Local strBoolBinop:TDelegate = binop.curry(TDelegate.Make(UnpackStr))
		Local boolBoolBinop:TDelegate = binop.curry(TDelegate.Make(UnpackBool))
		
		addPrim prims, "=", numBoolBinop.curry(TDelegate.Make(_numEq))
		addPrim prims, "<", numBoolBinop.curry(TDelegate.Make(_numLt))
		addPrim prims, ">", numBoolBinop.curry(TDelegate.Make(_numGt))
		addPrim prims, "/=", numBoolBinop.curry(TDelegate.Make(_numNe))
		addPrim prims, "<=", numBoolBinop.curry(TDelegate.Make(_numLe))
		addPrim prims, ">=", numBoolBinop.curry(TDelegate.Make(_numGe))
		addPrim prims, "&&", boolBoolBinop.curry(TDelegate.Make(_boolAnd))
		addPrim prims, "||", boolBoolBinop.curry(TDelegate.Make(_boolOr))
		addPrim prims, "string=?", strBoolBinop.curry(TDelegate.Make(_strEq))
		addPrim prims, "string<?", strBoolBinop.curry(TDelegate.Make(_strLt))
		addPrim prims, "string>?", strBoolBinop.curry(TDelegate.Make(_strGt))
		addPrim prims, "string<=?", strBoolBinop.curry(TDelegate.Make(_strLe))
		addPrim prims, "string>=?", strBoolBinop.curry(TDelegate.Make(_strGe))
		
		addPrim prims, "car", TDelegate.Make(Car)
		addPrim prims, "cdr", TDelegate.Make(Cdr)
		addPrim prims, "cons", TDelegate.Make(Cons)
		
		addPrim prims, "eqv?", TDelegate.Make(EqvP)
		addPrim prims, "eq?", TDelegate.Make(EqvP)	'eq? and equal? are allowed to be the same as eqv?, so they are
		addPrim prims, "equal?", TDelegate.Make(EqvP)
		
		Local mp:TDelegate = TDelegate.Make(_makePort), cp:TDelegate = TDelegate.Make(_closePort)
		addPrim prims, "open-input-file", mp.curry("READMODE")
		addPrim prims, "open-output-file", mp.curry("WRITEMODE")
		addPrim prims, "close-input-port", cp
		addPrim prims, "close-output-port", cp
		addPrim prims, "read", TDelegate.Make(_read)
		addPrim prims, "write", TDelegate.Make(_write)
		addPrim prims, "read-all", TDelegate.Make(_readAll)
		addPrim prims, "read-contents", TDelegate.Make(_readContents)
		addPrim prims, "print", TDelegate.Make(_print)
		addPrim prims, "apply", TDelegate.Make(_apply)
		
		Return prims
	End Function
End Type

