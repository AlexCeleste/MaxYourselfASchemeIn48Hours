
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
Include "SchemeBuiltins2.bmx"

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
							Then Return Eval(env, LispVal(C._)) ..
							Else Return Eval(env, LispVal(B._))
						
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
						
					Case unCons(unVal("if"), RefCell.Any).match(val), ..
					     unCons(unVal("set!"), RefCell.Any).match(val), ..
					     unCons(unVal("define"), RefCell.Any).match(val), ..
					     unCons(unVal("lambda"), RefCell.Any).match(val)
						badSpecialForm val
						
					Case unCons(A, B).match(val)
						Return Apply(Eval(env, LispVal(A._)), ConsList.Map(ev.curry(env), ConsList(B._)))
					Case unCons(A, Null).match(val)
						Return Apply(Eval(env, LispVal(A._)), Null)
						
				End Select
				Function badSpecialForm(val:LispVal)
					LispError.Raise "Eval: malformed '" + LispList(val).vals.val.ToString() + "' expression: " + val.ToString()
				End Function
				
			Case LispNum(val), LispString(val), LispBool(val), LispChar(val), LispVector(val)
				Return val
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
				Return LispVal(ConsList.Last(ConsList.Map(ev.curry(newEnv), f.body)))
				
			Default ; LispError.Raise "Apply: cannot apply non-function '" + op.ToString() + "'"
		End Select
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
		Print "-> " + v.ToString()
		Return Null
	End Function
End Type


Local read:TDelegate = TDelegate.Make(SchemeREPL.Read), eval:TDelegate = TDelegate.Make(SchemeREPL.eval), write:TDelegate = TDelegate.Make(SchemeREPL.Write)

Local q:SchemeParser = New SchemeParser, globalEnv:SchemeEnv = SchemeEnv.MakeGlobal()
Local _main:TDelegate = write.compose(eval.curry(globalEnv).compose(read.curry(q)))

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
