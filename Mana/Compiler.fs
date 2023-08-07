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
                | Bool true -> Ok thenCode
                | Bool false -> Ok elseCode
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
                | Some(Native handler) -> Ok handler
                | Some _ -> Error RuntimeError.NotAFunction
                | None -> Error RuntimeError.FunctionNotFound

            let mutable argsValues = List.empty
            let mutable env = env

            for arg in args do
                let! argEnv, argValue = arg env
                env <- argEnv
                argsValues <- argValue :: argsValues

            let! v = handler argsValues

            return env, v
        })
}

and compileExpr (expr: Expr) : CompileResult<Code> =
    match expr with
    | Expr.Unit -> compileUnit
    | NumberLiteral n -> Ok(fun env -> Ok(env, n |> Num))
    | StringLiteral s -> Ok(fun env -> Ok(env, s |> Str))
    | Ident name -> compileIdent name
    | Call(name, args) -> compileCall name args
    | Lambda(l, expr) -> failwith "todo"
    | Match(expr, matchCases) -> failwith "todo"
    | Block exprs -> compileBlock exprs
    | List patterns -> failwith "todo"
    | Dict tuples -> failwith "todo"
    | If(condExpr, thenExpr, elseExpr) -> compileIf condExpr thenExpr elseExpr
    | Let(s, expr) -> compileLet s expr
