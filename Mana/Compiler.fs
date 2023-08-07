module Mana.Compiler

open Mana
open Mana.Ast
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

and compileExprs =
    function
    | [] -> compileUnit |> Result.map List.singleton
    | [ expr ] -> compileExpr expr |> Result.map List.singleton
    | head :: tail -> result {
        let! headCode = compileExpr head
        let! tailCode = compileExprs tail

        return headCode :: tailCode
      }

and compileCall name args = result {
    let! args = compileExprs args

    return
        (fun env -> result {
            let! handler =
                match (Env.get name env) with
                | Some(Fun handler) -> Ok handler
                | Some _ -> Error RuntimeError.NotAFunction
                | None -> Error RuntimeError.FunctionNotFound

            let mutable argsValues = List.empty
            let mutable env = env

            for arg in args do
                let! argEnv, argValue = arg env
                env <- argEnv
                argsValues <- argValue :: argsValues

            let! env, v = handler env argsValues

            return env, v
        })
}

and compileClosure (argsNames: string list) body = result {
    let! body = compileExpr body

    return
        (fun env -> result {
            let closure =
                Value.Fun(fun env (args: Value list) -> result {

                    // Check arity
                    if argsNames.Length <> args.Length then
                        return! Error RuntimeError.InvalidArgumentCount

                    // Prepare env with args
                    let scope =
                        (argsNames, args)
                        ||> List.zip
                        |> List.fold (fun env (k, v) -> Env.set k v env) (Env.scoped env)

                    let! env, v = body scope
                    return env, v
                })

            return env, closure
        })
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
                exprValues <- exprValue :: exprValues

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

and compileExpr (expr: Expr) : CompileResult<Code> =
    match expr with
    | Unit -> compileUnit
    | Bool b -> Ok(fun env -> Ok(env, b |> Value.Bool))
    | Num n -> Ok(fun env -> Ok(env, n |> Value.Num))
    | Str s -> Ok(fun env -> Ok(env, s |> Value.Str))
    | Ident name -> compileIdent name
    | Call(name, args) -> compileCall name args
    | Closure(args, body) -> compileClosure args body
    | Match(expr, matchCases) -> failwith "todo"
    | Block exprs -> compileBlock exprs
    | List exprs -> compileList exprs
    | Table pairs -> compileTable pairs
    | If(condExpr, thenExpr, elseExpr) -> compileIf condExpr thenExpr elseExpr
    | Let(s, expr) -> compileLet s expr
