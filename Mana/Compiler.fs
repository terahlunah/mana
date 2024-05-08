module Mana.Compiler

open Mana
open Mana.Error
open Microsoft.FSharp.Core
open Mana.Pattern

type Callable = Env<Value> -> Value

// Execution functions

let rec bindPattern (env: Env<Value>) (value: Value) (pattern: Pattern) : bool =
    match pattern with
    | Pattern.Wildcard -> true
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
                    | v::vs, Single p::ps ->
                        bindPattern env v p && bindList vs ps
                    | vs, Rest p::ps ->
                        let rest = vs |> List.take restSize
                        env.set(p, Value.List rest)
                        bindList (vs |> List.skip restSize) ps
                    | _ -> false
                    
                bindList values patterns
            else
                false
        | _ -> false
    | Pattern.Table patterns -> failwith "todo"

let execNil (env: Env<Value>) = Value.Nil
let execBool b (env: Env<Value>) = Value.Bool b
let execNum n (env: Env<Value>) = Value.Num n
let execStr s (env: Env<Value>) = Value.Str s

let execBlock body (env: Env<Value>) =
    body
    |> List.map (fun e -> e env)
    |> List.tryLast
    |> Option.defaultValue Value.Nil
    
let execAssign symbol value (env: Env<Value>) =
    let v = value env
    
    if env.globalAssign(symbol, v) then
        Value.Nil
    else
        raiseError (ManaError.SymbolNotFound symbol)
        
let execLet pattern value (env: Env<Value>) =
    let v = value env

    let letEnv = env.localScope ("let")

    if bindPattern letEnv v pattern then
        env.merge letEnv
        Value.Nil
    else
        raiseError (ManaError.PatternMatchingFailed)
    
let execCall name args (env: Env<Value>) =
    match (env.get name) with
    | Some(Value.Closure handler) ->
        let eval e = e env
        let args = List.map eval args
        handler env args
    | Some e -> e
    | None -> raiseError (ManaError.UnknownSymbol name)

let execClosure name (paramNames: string list) body (env: Env<Value>) =
    let closureHandler _ (args: Value list) =
        // Check arity
        if paramNames.Length <> args.Length then
            raiseError (ManaError.InvalidArgumentCount("closure", args.Length, paramNames.Length))

        // Prepare env with args
        let scope = env.localScope name

        for k, v in List.zip paramNames args do
            scope.set (k, v)

        body scope
    Value.Closure closureHandler

let execTable items (env: Env<Value>)=
    let eval (k, v) = k env, v env
    items |> List.map eval |> Map |> Value.Table
    
let execList items (env: Env<Value>)=
    items
        |> List.collect
        (function
        | Elem e -> e env |> List.singleton
        | Splat splat ->
            match splat env with
            | List l -> l
            | _ -> raiseError ManaError.OnlyListsCanBeSplatted
        ) |> Value.List    
let execMatch value patterns cases (env: Env<Value>)=
    let v = value env
        
    let rec runMatch cases =
        match cases with
        | [] -> raiseError (ManaError.PatternMatchingFailed)
        | (pattern, guard, body)::tail ->
            // let caseEnv = env.localScope ($"match case |%A{pattern}|")
            let caseEnv = env.localScope ("match case")
            if bindPattern caseEnv v pattern then
                let guardOk = guard |> Option.map (fun g -> g caseEnv |> Value.isTrue) |> Option.defaultValue true
                    
                if guardOk then
                    env.merge caseEnv
                    body env
                else
                    runMatch tail
            else
                runMatch tail
    
    runMatch cases

// Compilation functions

let rec compileCall name args : Callable =
    let args = compileExprs args
    
    execCall name args

and compileClosure (paramNames: string list) body : Callable =
    let body = compileExpr body
    let closureName = $"""closure |{paramNames |> String.concat ","}|"""

    execClosure closureName paramNames body

and compileList items : Callable =
    let items =
        items
        |> List.map (
            function
            | Elem e -> compileExpr e |> Elem
            | Splat splat -> compileExpr splat |> Splat
        )

    execList items

and compileTable items : Callable =
    let items =
        items
        |> List.map (fun (k, v) -> compileExpr k, compileExpr v)

    execTable items

and compileBlock body : Callable =
    let body = compileExprs body

    execBlock body

and compileAssign (symbol: string) (value: Ast): Callable =
    let value = compileExpr value

    execAssign symbol value

and compileLet (p: Pattern) (value: Ast): Callable =
    let value = compileExpr value

    execLet p value

and compileMatch (value: Ast) (cases: MatchCase list) : Callable =
    let value = compileExpr value
    
    let patterns = cases |> List.map _.pattern
    let conds = cases |> List.map _.guard |> List.map (Option.map compileExpr)
    let bodies = cases |> List.map _.body |> compileExprs
    let cases = List.zip3 patterns conds bodies
    
    execMatch value patterns cases

and compileExpr (expr: Ast) : Callable =
    match expr with
    | Ast.Nil -> execNil
    | Ast.Bool b -> execBool b
    | Ast.Num n -> execNum n
    | Ast.Str s -> execStr s
    | Ast.Call(name, args) -> compileCall name args
    | Ast.Closure(args, body) -> compileClosure args body
    | Ast.List items -> compileList items
    | Ast.Table pairs -> compileTable pairs
    | Ast.Block body -> compileBlock body
    | Ast.Assign(symbol, value) -> compileAssign symbol value
    | Ast.Let(pattern, value) -> compileLet pattern value
    | Ast.Match(value, cases) -> compileMatch value cases

and compileExprs exprs : Callable list = List.map compileExpr exprs

