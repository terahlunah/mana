module Mana.Compiler

open System
open Mana
open Mana.Parser
open FsToolkit.ErrorHandling

type Code = Env<Value> -> RuntimeResult<Env<Value> * Value>

let rec compileUnit = Ok(fun env -> Ok(env, Value.Unit))

and compileLet s expr =
    expr
    |> compileExpr
    |> Result.map (fun f ->
        (fun env ->
            f env
            |> Result.map (fun (env, v) -> env |> Env.set s v, Value.Unit)
        )
    )

and compileIdent name =
    Ok(fun env ->
        env
        |> Env.get name
        |> Option.map (fun v -> env, v)
        |> Option.okOr RuntimeError.UnknownIdent
    )

and compileIf condExpr thenExpr elseExpr = result {
    let! condCode = compileExpr condExpr
    let! thenCode = compileExpr thenExpr
    let! elseCode = compileExpr elseExpr

    return
        (fun env -> result {
            let! env, condTest = condCode env

            let! branch =
                match condTest with
                | Value.Bool true -> Ok thenCode
                | Value.Bool false -> Ok elseCode
                | _ -> Error RuntimeError.InvalidArguments

            let! env, branchValue = branch env
            return env, branchValue
        })
}

and compileBlock =
    function
    | [] -> compileUnit
    | [ expr ] -> compileExpr expr
    | expr :: block -> result {
        let! exprCode = compileExpr expr
        let! blockCode = compileBlock block

        return
            (fun env -> result {
                let! env, _ = exprCode env
                return! blockCode env
            })
      }

and compileExprs exprs = List.traverseResultM compileExpr exprs

and compileCall name args = result {
    let! args = compileExprs args

    return
        (fun env -> result {
            let! handler =
                match (Env.get name env) with
                | Some(Fun handler) -> Ok handler
                | Some _ -> Error RuntimeError.NotAFunction
                | None -> Error(RuntimeError.FunctionNotFound name)

            let mutable argsValues = List.empty
            let mutable env = env

            // Use foldResult
            for arg in args do
                let! argEnv, argValue = arg env
                env <- argEnv
                argsValues <- argsValues @ [ argValue ]

            let! env, v = handler env argsValues

            return env, v
        })
}

and compileClosure (argsNames: Argument list) body = result {
    let! body = compileExpr body
    let argLength = argsNames.Length

    let closure =
        Value.Fun(fun env (args: Value list) -> result {

            // Check arity
            if argLength <> args.Length then
                return! Error RuntimeError.InvalidArgumentCount

            // Prepare env with args
            let scope =
                (argsNames, args)
                ||> List.zip
                |> List.fold (fun env (Argument k, v) -> Env.set k v env) (Env.localScope env)

            let! env, v = body scope
            return env, v
        })

    return (fun env -> result { return env, closure })
}

and compileList exprs = result {

    let! exprs = compileExprs exprs

    return
        (fun env -> result {
            let mutable exprValues = List.empty
            let mutable env = env

            for expr in exprs do
                let! exprEnv, exprValue = expr env
                env <- exprEnv
                exprValues <- exprValues @ [ exprValue ]

            let v = Value.List exprValues

            return env, v
        })
}

and compileTable pairs = result {

    let keys = pairs |> List.map fst
    let values = pairs |> List.map snd
    let! values = compileExprs values

    let pairs = List.zip keys values

    return
        (fun env -> result {
            let mutable items = Map.empty
            let mutable env = env

            for key, value in pairs do
                let! exprEnv, value = value env
                env <- exprEnv
                items <- items |> Map.add key value

            let v = Value.Table items

            return env, v
        })
}

and compileMatch expr (cases: MatchCase list) = result {
    let! exprCode = compileExpr expr
    let patterns = cases |> List.map (fun x -> x.pattern)
    let! codes = cases |> List.traverseResultM (fun x -> compileExpr x.body)

    return
        (fun env -> result {
            let! env, value = exprCode env

            return!
                List.zip patterns codes
                |> findMatch (env |> Env.localScope) value
        })
}

and findMatch env value cases =
    match cases with
    | [] -> Error RuntimeError.PatternMatchingFailed
    | case :: cases ->
        let pattern, code = case
        let hasMatch, env = matchCase env value pattern
        if hasMatch then code env else findMatch env value cases

and matchCase env value (pattern: Pattern) : bool * Env<Value> =
    match pattern with
    | Underscore -> true, env
    | BoolPattern b -> value |> Value.isBool b, env
    | NumPattern n -> value |> Value.isNum n, env
    | StrPattern s -> value |> Value.isStr s, env
    | IdentPattern i -> true, Env.set i value env
    | ListPattern patterns ->
        match value with
        | Value.List values ->
            List.zip patterns values
            |> List.map (fun (pattern, value) -> matchCase env value pattern)
            |> List.fold
                (fun (accMatch, accEnv) (caseMatch, caseEnv) -> accMatch && caseMatch, Env.merge accEnv caseEnv)
                (true, Env.empty)
        | _ -> false, env

and compileExpr (expr: Expr) : CompileResult<Code> =
    match expr with
    | Unit -> compileUnit
    | Bool b -> Ok(fun env -> Ok(env, b |> Value.Bool))
    | Num n -> Ok(fun env -> Ok(env, n |> Value.Num))
    | Char c -> Ok(fun env -> Ok(env, c |> Value.Char))
    | Str s -> Ok(fun env -> Ok(env, s |> Value.Str))
    | Ident name -> compileIdent name
    | Call(name, args) -> compileCall name args
    | Closure(args, body) -> compileClosure args body
    | Match(expr, cases) -> compileMatch expr cases
    | Block exprs -> compileBlock exprs
    | List exprs -> compileList exprs
    | Table pairs -> compileTable pairs
    | If(condExpr, thenExpr, elseExpr) -> compileIf condExpr thenExpr elseExpr
    | Let(s, expr) -> compileLet s expr

let compileDefinition (d: Definition) (m: Module) staticEnv : CompileResult<_> = result {

    let! body = compileExpr d.body

    let qualifiedName =
        if m.name <> String.Empty then
            $"{m.name}.{d.name}"
        else
            d.name

    let defArgs = d.args
    let argsLen = d.args.Length

    let f =
        Value.Fun(fun env (args: Value list) -> result {

            // Check arity
            if argsLen <> args.Length then
                return! Error RuntimeError.InvalidArgumentCount

            // Prepare env with args
            let scope =
                (defArgs, args)
                ||> List.zip
                |> List.fold (fun env (Argument k, v) -> Env.set k v env) (Env.localScope env)

            let! env, v = body scope
            return env, v
        })

    return staticEnv |> Env.set qualifiedName f
}

let compileModule (m: Module) staticEnv : CompileResult<_> =
    m.definitions
    |> List.foldResult (fun staticEnv d -> compileDefinition d m staticEnv) staticEnv

let compileProgram (program: Program) staticEnv : CompileResult<_> =
    program.modules
    |> List.foldResult (fun staticEnv m -> compileModule m staticEnv) staticEnv
