module Mana.Compiler

open Mana
open Mana.Error
open Yute

type Callable = Env<Value> -> Value
let rec compileUnit = fun env -> Value.Nil

and compileCall name args : Callable =
    let args = compileExprs args

    fun env ->
        match (env.get name) with
        | Some(Value.Closure handler) ->
            let eval e = e env
            let args = args |> List.map eval

            handler env args
        | Some e -> e
        | None -> raiseError (ManaError.UnknownSymbol name)

and compileClosure (paramNames: string list) body : Callable =
    let body = compileExpr body

    let closure =
        fun (env: Env<Value>) (args: Value list) ->
            // Check arity
            if paramNames.Length <> args.Length then
                raiseError (ManaError.InvalidArgumentCount("closure", args.Length, paramNames.Length))

            // Prepare env with args
            let scope = env.localScope ()

            for k, v in List.zip paramNames args do
                scope.set (k, v)

            body scope
        |> Value.Closure

    fun _ -> closure

and compileList exprs : Callable =
    let exprs = compileExprs exprs

    fun env ->
        let eval e = e env
        exprs |> List.map eval |> Value.List

and compileTable items : Callable =

    let items =
        items
        |> List.map (fun (k, v) -> compileExpr k, compileExpr v)

    fun env ->
        let eval (k, v) = k env, v env
        items |> List.map eval |> Map |> Value.Table

and compileBlock body : Callable =
    let body = compileExprs body

    fun env ->
        body
        |> List.map (fun e -> e env)
        |> List.tryLast
        |> Option.defaultValue Value.Nil

and compileLet (p: Pattern) (value: Ast) : Callable =
    let value = compileExpr value

    fun env ->
        let v = value env

        let letEnv = env.localScope ()

        if bindPattern letEnv v p then
            Value.Nil
        else
            raiseError (ManaError.PatternMatchingFailed)

and compileMatch (value: Ast) (cases: MatchCase list) : Callable =
    let value = compileExpr value
    
    let patterns = cases |> List.map _.pattern
    let bodies = cases |> List.map _.body |> compileExprs
    let cases = List.zip patterns bodies
    
    fun env ->
        let v = value env
        
        let rec runMatch cases =
            match cases with
            | [] -> raiseError (ManaError.PatternMatchingFailed)
            | (pattern, body)::tail ->
                let caseEnv = env.localScope ()
                if bindPattern caseEnv v pattern then
                    body caseEnv
                else
                    runMatch tail
        
        runMatch cases

and bindPattern (env: Env<Value>) (value: Value) (pattern: Pattern) : bool =
    match pattern with
    | Pattern.Underscore -> true
    | Pattern.Nil -> value |> Value.isNil
    | Pattern.Bool b -> value |> Value.isBool b
    | Pattern.Num n -> value |> Value.isNum n
    | Pattern.Str s -> value |> Value.isStr s
    | Pattern.Symbol s ->
        env.set (s, value)
        true
    | Pattern.List patterns ->
        match value with
        | Value.List values ->
            let bind = bindPattern env |> uncurry
            List.zip values patterns |> List.forall bind
        | _ -> false
    | Pattern.Table patterns -> failwith "todo"

and compileExpr (expr: Ast) : Callable =
    match expr with
    | Ast.Nil -> compileUnit
    | Ast.Bool b -> fun env -> b |> Value.Bool
    | Ast.Num n -> fun env -> n |> Value.Num
    | Ast.Str s -> fun env -> s |> Value.Str
    | Ast.Call(name, args) -> compileCall name args
    | Ast.Closure(args, body) -> compileClosure args body
    | Ast.List exprs -> compileList exprs
    | Ast.Table pairs -> compileTable pairs
    | Ast.Block body -> compileBlock body
    | Ast.Let(pattern, value) -> compileLet pattern value
    | Ast.Match(value, cases) -> compileMatch value cases

and compileExprs exprs : Callable list = List.map compileExpr exprs
