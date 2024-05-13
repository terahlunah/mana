module Mana.Compiler

open Mana
open Mana.Error
open Microsoft.FSharp.Core
open Mana.Cont

type Executor = Context<Value> -> Env<Value> -> (Value -> Value) -> Value

// Utils

module List =
    let rec mapK f l k =
        match l with
        | [] -> k []
        | x :: xs -> f x (fun r -> mapK f xs (fun rs -> k (r :: rs)))

let rec bindPattern (env: Env<Value>) (value: Value) (pattern: Pattern) : bool =
    match pattern with
    | Pattern.Any -> true
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

            if not <| Pattern.isCollectionPatternValid patterns then
                raiseError ManaError.MoreThanOneRestPattern

            if Pattern.matchesSize values.Length patterns then
                let restSize = values.Length - Pattern.minimumSize patterns

                let rec bindList vs ps =
                    match vs, ps with
                    | [], [] -> true
                    | v :: vs, Single p :: ps -> bindPattern env v p && bindList vs ps
                    | vs, Rest p :: ps ->
                        let rest = vs |> List.take restSize
                        env.set (p, Value.List rest)
                        bindList (vs |> List.skip restSize) ps
                    | _ -> false

                bindList values patterns
            else
                false
        | _ -> false
    | Pattern.Table patterns -> todo "Table pattern matching"

// Execution functions

let execBlock (body: Executor list) (ctx:Context<Value>) (env: Env<Value>) = cps {
    let! rs = body |> List.mapK (fun e -> e ctx env)
    return rs |> List.tryLast |> Option.defaultValue Value.Nil
}

let execAssign symbol (valueExec: Executor) (ctx:Context<Value>) (env: Env<Value>) = cps {
    let! v = valueExec ctx env

    if env.globalAssign (symbol, v) then
        return Value.Nil
    else
        return raiseError (ManaError.SymbolNotFound symbol)
}

let execLet pattern (valueExec: Executor) (ctx:Context<Value>) (env: Env<Value>) = cps {
    let! v = valueExec ctx env

    let letEnv = env.localScope "let"

    if bindPattern letEnv v pattern then
        env.merge letEnv
        return Value.Nil
    else
        return raiseError (ManaError.PatternMatchingFailed)
}

let execCall name args (ctx:Context<Value>) (env: Env<Value>) = cps {
    match (env.get name) with
    | Some(Value.Closure handler) ->
        let exec e = e ctx env
        let! args = List.mapK exec args
        return! handler ctx env args
    | Some e -> return e
    | None -> return raiseError (ManaError.UnknownSymbol name)
}

let execClosure name (paramNames: string list) body (ctx:Context<Value>) (env: Env<Value>) = cps {
    let closureHandler ctx _ (args: Value list) = cps {
        // Check arity
        if paramNames.Length <> args.Length then
            return raiseError (ManaError.InvalidArgumentCount("closure", args.Length, paramNames.Length))
        else
            // Prepare env with args
            let scope = env.localScope name

            List.zip paramNames args |> List.iter scope.set

            return! body ctx scope
    }

    return Value.Closure closureHandler
}

let execTable keys values (ctx:Context<Value>) (env: Env<Value>) = cps {
    let exec e = e ctx env
    let! keys = List.mapK exec keys
    let! values = List.mapK exec values
    return List.zip keys values |> Map |> Value.Table
}

let execList items (ctx:Context<Value>) (env: Env<Value>) = cps {
    let! items =
        List.mapK
            (function
            | Elem executor -> cps {
                let! e = executor ctx env
                return List.singleton e
              }
            | Splat splat -> cps {
                let! e = splat ctx env

                match e with
                | List l -> return l
                | _ -> return raiseError ManaError.OnlyListsCanBeSplatted
              })
            items

    let items = List.concat items
    return Value.List items
}

let execMatch valueExecutor patterns cases (ctx:Context<Value>) (env: Env<Value>) = cps {
    let! v = valueExecutor ctx env

    let rec runMatch cases = cps {
        match cases with
        | [] -> return raiseError (ManaError.PatternMatchingFailed)
        | (pattern, guard, body) :: tail ->
            // let caseEnv = env.localScope ($"match case |%A{pattern}|")
            let caseEnv = env.localScope ("match case")

            if bindPattern caseEnv v pattern then
                let! guardOk = 
                    match guard with
                    | Some guardExecutor ->
                        cps {
                            let! pass = guardExecutor ctx caseEnv
                            return Value.isTrue pass
                        }
                    | None -> cps { return true }

                if guardOk then
                    env.merge caseEnv
                    return! body ctx env
                else
                    return! runMatch tail
            else
                return! runMatch tail
    }

    return! runMatch cases
}

// Compilation functions

let rec compileCall name args : Executor =
    let args = compileExprs args

    execCall name args

and compileClosure (paramNames: string list) body : Executor =
    let body = compileExpr body
    let closureName = $"""closure |{paramNames |> String.concat ","}|"""

    execClosure closureName paramNames body

and compileList items : Executor =
    let items =
        items
        |> List.map (
            function
            | Elem e -> compileExpr e |> Elem
            | Splat splat -> compileExpr splat |> Splat
        )

    execList items

and compileTable items : Executor =
    let keys = items |> List.map (fun (k, _) -> compileExpr k)
    let values = items |> List.map (fun (_, v) -> compileExpr v)

    execTable keys values

and compileBlock body : Executor =
    let body = compileExprs body

    execBlock body

and compileAssign (symbol: string) (value: Expr) : Executor =
    let value = compileExpr value

    execAssign symbol value

and compileLet (p: Pattern) (value: Expr) : Executor =
    let value = compileExpr value

    execLet p value

and compileMatch (value: Expr) (cases: MatchCase<Expr> list) : Executor =
    let value = compileExpr value

    let patterns = cases |> List.map _.pattern
    let conds = cases |> List.map _.guard |> List.map (Option.map compileExpr)
    let bodies = cases |> List.map _.body |> compileExprs
    let cases = List.zip3 patterns conds bodies

    execMatch value patterns cases

and compileExpr (expr: Expr) : Executor =
    match expr with
    | Expr.Nil -> fun _ _ -> cps { return Value.Nil }
    | Expr.Bool b -> fun _ _ -> cps { return Value.Bool b }
    | Expr.Num n -> fun _ _ -> cps { return Value.Num n }
    | Expr.Str s -> fun _ _ -> cps { return Value.Str s }
    | Expr.Call(name, args) -> compileCall name args
    | Expr.Closure(args, body) -> compileClosure args body
    | Expr.List items -> compileList items
    | Expr.Table pairs -> compileTable pairs
    | Expr.Block body -> compileBlock body
    | Expr.Assign(symbol, value) -> compileAssign symbol value
    | Expr.Let(pattern, value) -> compileLet pattern value
    | Expr.Match(value, cases) ->  compileMatch value cases

and compileExprs exprs : Executor list = List.map compileExpr exprs

let compile (expr: Expr) =
    let executor = compileExpr expr
    fun ctx env -> executor ctx env id
