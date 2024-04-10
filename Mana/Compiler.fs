module Mana.Compiler

open Mana
open Mana.Error

type Callable = Env<Value> -> Value
let rec compileUnit = fun env -> Value.Nil

and compileLet name expr : Callable =
    let expr = compileExpr expr

    fun env ->
        let v = expr env
        env.set (name, v)
        Value.Nil

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
    let body = compileExprs body

    let closure =
        fun (env: Env<Value>) (args: Value list) ->
            // Check arity
            if paramNames.Length <> args.Length then
                raiseError (ManaError.InvalidArgumentCount("closure", args.Length, paramNames.Length))

            // Prepare env with args
            let scope = env.localScope ()

            for k, v in List.zip paramNames args do
                scope.set (k, v)

            let run x = x scope

            body
            |> List.map run
            |> List.tryLast
            |> Option.defaultValue Value.Nil
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
    | Ast.Let(s, expr) -> compileLet s expr

and compileExprs = List.map compileExpr

and compileScript (script: Ast list) : Callable =
    let exprs = compileExprs script

    fun env ->
        exprs
        |> List.map (fun e -> e env)
        |> List.tryLast
        |> Option.defaultValue Value.Nil
