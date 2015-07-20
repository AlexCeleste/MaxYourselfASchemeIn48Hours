
' Max Yourself a Scheme in 48 Hours
' a translation/reworking from Jonathan Tang's Haskell original
' translated by Alex G ("Yasha")
' licence is CC BY-SA 3.0

' This is the code to the "finished" Scheme interpreter
' If you haven't read through the exercises, go back and look at them first!

SuperStrict

Import "TMeta.bmx"
Import "Functional.bmx"

Include "SchemeParser2.bmx"
Include "SchemeTypes.bmx"
Include "SchemeBuiltins3.bmx"

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


Type SchemeEnv
	Field _local:TMap, _closure:SchemeEnv
	Method Copy:SchemeEnv()
		Local c:SchemeEnv = Make()
		c._local = _local.Copy() ; If _closure Then c._closure = _closure.Copy()
		Return c
	End Method
	Function Make:SchemeEnv(closure:SchemeEnv = Null)
		Local e:SchemeEnv = New SchemeEnv ; e._local = CreateMap() ; e._closure = closure ; Return e
	End Function
	Function MakeGlobal:SchemeEnv()
		Return Make(SchemeBuiltins.prims.Copy())
	End Function
	Function IsBound:Int(env:SchemeEnv, name:String)
		If env = Null Then Return 0
		Return env._local.Contains(name) Or IsBound(env._closure, name)
	End Function
	Function GetVar:LispVal(env:SchemeEnv, name:String)
		If env = Null Then LispError.Raise "cannot get undefined variable '" + name + "'"
		Local val:Object = env._local.ValueForKey(name) ; If val = Null Then val = GetVar(env._closure, name)
		Return LispVal(val)
	End Function
	Function SetVar:LispVal(env:SchemeEnv, name:String, val:LispVal)
		If env = Null Then LispError.Raise "cannot set undefined variable '" + name + "'"
		If Not env._local.Contains(name) Then SetVar env._closure, name, val Else env._local.Insert name, val
		Return val
	End Function
	Function DefineVar:LispVal(env:SchemeEnv, name:String, val:LispVal)
		env._local.Insert(name, val) ; Return val
	End Function
	Function BindVars:SchemeEnv(env:SchemeEnv, bindings:ConsList)
		Global addBinding:TDelegate = TDelegate.Make(_)
		Function _:SchemeEnv(env:SchemeEnv, binding:Object[])
			DefineVar env, String(binding[0]), LispVal(binding[1]) ; Return env
		End Function
		Return SchemeEnv(ConsList.FoldL(addBinding.curry(env), env, bindings))
	End Function
End Type


Type SchemeREPL
	Function Eval:LispVal(env:SchemeEnv, val:LispVal)
		Local ret:LispVal, tc:LispDeferredTailCall
		Repeat
			ret = TailEval(env, val) ; tc = LispDeferredTailCall(ret)
			If tc
				env = tc.env ; val = tc.val ; ret = Null
			EndIf
		Until ret
		Return ret
	End Function
	
	Function EvalMany:LispVal(env:SchemeEnv, vals:LispVal[])
		Local ret:LispVal
		For Local v:LispVal = EachIn vals
			ret = Eval(env, v)
		Next
		Return ret
	End Function
	
	Function TailEval:LispVal(env:SchemeEnv, val:LispVal)
		Global ev:TDelegate = TDelegate.Make(Eval), unVal:RefCell(_:Object) = RefCell.unVal, unCons:RefCell(l:Object, r:RefCell) = RefCell.unCons
		Global unType:RefCell(_:Object, t:Object(_:Object)) = RefCell.unType, unMaybe:RefCell(_:RefCell) = RefCell.unMaybe
		Global isAtom:TDelegate = TDelegate.Make(LispAtom.Is)
		Local A:RefCell = RefCell.Make(), B:RefCell = New A, C:RefCell = New A', D:RefCell = New A
		
		Select val
			Case LispAtom(val)
				Return SchemeEnv.GetVar(env, LispAtom(val).name)
				
			Case LispList(val)
				Select True
					Case unCons(unVal("quote"), B).match(val)
						Return LispVal(ConsList(B._).val)
						
					Case unCons(unVal("if"), unCons(A, unCons(B, unCons(C, Null)))).match(val)
						Local pred:LispVal = Eval(env, LispVal(A._))
						If LispBool(pred) And (LispBool(pred).val = 0) ..
							Then Return LispDeferredTailCall.Make(env, LispVal(C._)) ..
							Else Return LispDeferredTailCall.Make(env, LispVal(B._))
						
					Case unCons(unVal("set!"), unCons(unType(A, LispAtom.Is), unCons(B, Null))).match(val)
						Return SchemeEnv.SetVar(env, LispAtom(A._).name, Eval(env, LispVal(B._)))
						
					Case unCons(unVal("define"), unCons(unType(A, LispAtom.Is), unCons(B, Null))).match(val)
						Return SchemeEnv.DefineVar(env, LispAtom(A._).name, Eval(env, LispVal(B._)))
					Case unCons(unVal("define"), unCons(unType(A, LispDottedList.Is), B)).match(val)
						Local nargs:LispDottedList = LispDottedList(A._), name:Object = nargs.vals.val
						Local f:LispFunc = LispFunc.Make(nargs.vals.nx, nargs.last.ToString(), ConsList(B._), env)
						Return SchemeEnv.DefineVar(env, name.ToString(), f)
					Case unCons(unVal("define"), unCons(unType(A, LispList.Is), B)).match(val)
						Local nargs:LispList = LispList(A._), name:Object = nargs.vals.val
						Local f:LispFunc = LispFunc.Make(nargs.vals.nx, Null, ConsList(B._), env)
						Return SchemeEnv.DefineVar(env, name.ToString(), f)
						
					Case unCons(unVal("lambda"), unCons(unType(A, LispDottedList.Is), B)).match(val)
						Local args:LispDottedList = LispDottedList(A._)
						Return LispFunc.Make(args.vals, args.last.ToString(), ConsList(B._), env)
					Case unCons(unVal("lambda"), unCons(unType(A, LispList.Is), B)).match(val)
						Return LispFunc.Make(LispList(A._).vals, Null, ConsList(B._), env)
					Case unCons(unVal("lambda"), unCons(unType(A, LispAtom.Is), B)).match(val)
						Return LispFunc.Make(Null, B._.ToString(), ConsList(B._), env)
						
					Case unCons(unVal("macro"), B).match(val)
						Global lam:LispAtom = LispAtom.Make("lambda")
						Local ll:LispList = LispList.FromCons(ConsList.Cons(lam, ConsList(B._)))
						Return LispMacro.FromFunc(LispFunc(TailEval(env, ll)))
						
					Case unCons(unVal("load"), unCons(unType(A, LispString.Is), Null)).match(val)
						Local port:LispPort = LispPort.Make("READMODE", LispString(A._).val)
						ConsList.Map(ev.curry(env), SchemeBuiltins._readAll(ConsList.Cons(port, Null)).vals)
						port.stream.Close() ; port.stream = Null
						Return LispBool._True
						
					Case unCons(unVal("if"), RefCell.Any).match(val), ..
					     unCons(unVal("set!"), RefCell.Any).match(val), ..
					     unCons(unVal("load"), RefCell.Any).match(val), ..
					     unCons(unVal("macro"), RefCell.Any).match(val), ..
					     unCons(unVal("define"), RefCell.Any).match(val), ..
					     unCons(unVal("lambda"), RefCell.Any).match(val)
					DebugStop
						badSpecialForm val
						
					Case unCons(A, B).match(val)
						Local func:LispVal = Eval(env, LispVal(A._)), args:ConsList = ConsList(B._)
						If Not LispMacro(func) Then args = ConsList.Map(ev.curry(env), args)
						Return Apply(func, args)
					Case unCons(A, Null).match(val)
						Return Apply(Eval(env, LispVal(A._)), Null)
						
				End Select
				Function badSpecialForm(val:LispVal)
					LispError.Raise "Eval: malformed '" + LispList(val).vals.val.ToString() + "' expression: " + val.ToString()
				End Function
				
			Case LispNum(val), LispString(val), LispBool(val), LispChar(val), LispVector(val)
				Return val
			Case LispDeferredTailCall(val)
				LispError.Raise "Eval: deferred tail calls are not supposed to be used as values"
		End Select
		LispError.Raise "Eval: bad special form " + val.ToString()	'Getting here requires something to go wrong; all match branches return
	End Function
	
	Function Apply:LispVal(op:LispVal, args:ConsList)
		Select op
			Case LispPrimitiveFunc(op)
				Return LispVal(LispPrimitiveFunc(op).f.call(args))
				
			Case LispFunc(op)
				Local f:LispFunc = LispFunc(op), lnth:Int(_:ConsList) = ConsList.Length, fLen:Int = lnth(f.args)
				If (lnth(args) <> fLen And f.vararg = "") Or lnth(args) < fLen Then ..
				   LispError.ArgCount ConsList.Length(f.args), args
				Local newEnv:SchemeEnv = SchemeEnv.Make(f.closure)
				Global bind:TDelegate = TDelegate.Make(SchemeEnv.DefineVar), ev:TDelegate = TDelegate.Make(Eval)
				If f.vararg
					ConsList.ZipWith bind.curry(newEnv), f.args, ConsList.Take(args, fLen)
					bind.call2(f.vararg, ConsList.Drop(args, fLen))
				Else
					ConsList.ZipWith bind.curry(newEnv), f.args, args
				EndIf
				If LispMacro(f)
					Return LispVal(ConsList.Last(ConsList.Map(ev.curry(f.closure), ..
					                                          ConsList.Map(ev.curry(newEnv), f.body))))
				Else
					Local nonTail:ConsList = ConsList.Take(f.body, ConsList.Length(f.body) - 1)
					ConsList.Map(ev.curry(newEnv), nonTail)
					Return LispDeferredTailCall.Make(newEnv, LispVal(ConsList.Last(f.body)))
				EndIf
				
			Default ; LispError.Raise "Apply: cannot apply non-function '" + op.ToString() + "'"
		End Select
	End Function
	
	Function Read:LispVal[](p:SchemeParser, src:String)
		Try
			Local tree:TParseNode = p.Parse(SchemeLexer.Get().ScanString(src))
			Return p.ToLispVals(tree)
		Catch e:ParseError
			Local msg:String = e.ToString(), SRCH:String = "error trying to complete '"
			msg = msg.Replace(SRCH + "(", SRCH + "list").Replace(SRCH + "#", SRCH + "vector")
			msg = msg.Replace(SRCH + "'", SRCH + "quoted form").Replace(SRCH + "`", SRCH + "quasiquoted form")
			LispError.Raise msg
		Catch e:LexError
			LispError.Raise e.ToString()
		End Try
	End Function
	
	Function ReadOne:LispVal(p:SchemeParser, port:LispPort)
		Local vals:LispVal[]
		If port.cached
			vals = port.cached
		Else
			Local s:String ; While Not Eof(port.stream)
				s :+ port.stream.ReadLine() + "~n"
			Wend
			vals = SchemeREPL.Read(p, s)
		EndIf
		port.cached = vals[1..] ; Return vals[0]
	End Function
	
	Function Write:LispVal(p:LispPort, v:LispVal)
		p.stream.WriteLine(v.ToString())
		p.stream.Flush
		Return LispBool._True
	End Function
	
	Function Show:Object(v:LispVal)
		Print "-> " + (v.ToString()) ; Return Null
	End Function
	
	Function RunREPL:SchemeEnv(env:SchemeEnv = Null)
		Global read:TDelegate = TDelegate.Make(Read), eval:TDelegate = TDelegate.Make(EvalMany), write:TDelegate = TDelegate.Make(Show)
		Local q:SchemeParser = New SchemeParser
		If env = Null Then env = SchemeEnv.MakeGlobal()
		Local _main:TDelegate = write.compose(eval.curry(env).compose(read.curry(q)))
		Repeat
			Local in:String = Input("lisp>>> ")
			If in = "quit"
				Exit
			ElseIf in <> ""
				Try
					_main.call in
				Catch e:LispError
					Print e.ToString()
				End Try
			EndIf
		Forever
		Return env
	End Function
	
	Function RunFiles:SchemeEnv(files:String[], env:SchemeEnv = Null)
		Global read:TDelegate = TDelegate.Make(Read), eval:TDelegate = TDelegate.Make(Eval)
		If env = Null Then env = SchemeEnv.MakeGlobal()
		Local ev:TDelegate = eval.curry(env)
		For Local file:String = EachIn files
			Local port:LispPort = LispPort.Make("READMODE", file)
			ConsList.Map(ev, SchemeBuiltins._readAll(ConsList.Cons(port, Null)).vals)
			port.stream.Close() ; port.stream = Null
		Next
		Return env
	End Function
End Type


Local env:SchemeEnv
If AppArgs.Length > 1
	env = SchemeREPL.RunFiles(AppArgs[1..])
Else
	env = SchemeREPL.RunREPL()
EndIf

