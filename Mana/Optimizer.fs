module Mana.Optimizer

let rec desugar (ast: Ast) : Expr =
    match ast with
    | Ast.Nil -> Expr.Nil
    | Ast.Bool b -> Expr.Bool b
    | Ast.Num n -> Expr.Num n
    | Ast.Str s -> Expr.Str s
    | Ast.Call(name, args) -> Expr.Call(name, List.map desugar args)
    | Ast.List(items) ->
        items
        |> List.map (
            function
            | Elem e -> Elem(desugar e)
            | Splat splat -> Splat(desugar splat)
        )
        |> Expr.List
    | Ast.Table(items) ->
        items
        |> List.map (fun (k, v) -> desugar k, desugar v)
        |> Expr.Table
    | Ast.Let(name, value) -> Expr.Let(name, desugar value)
    | Ast.Assign(name, value) -> Expr.Assign(name, desugar value)
    | Ast.Match(value, cases) ->
        Expr.Match(
            desugar value,
            cases
            |> List.map (fun case -> {
                pattern = case.pattern
                guard = Option.map desugar case.guard
                body = desugar case.body
            })
        )
    | Ast.Closure(args, body) ->
        let args =
            if args.Length = 0 && Ast.useImplicitIt [ body ] then
                [ "it" ]
            else
                args

        Expr.Closure(args, desugar body)
    | Ast.Block body ->
        match body with
        | [ x ] -> desugar x
        | _ -> Expr.Block(List.map desugar body)
    | Ast.Binop(op, left, right) ->
        match op.symbol with
        | "?:" ->
            Expr.Match(
                desugar left,
                [
                    {
                        pattern = Pattern.Nil
                        body = desugar right
                        guard = None
                    }
                    {
                        pattern = Pattern.Symbol "x"
                        body = Expr.Call("x", [])
                        guard = None
                    }
                ]
            )
        | _ -> Expr.Call(op.handler, [ desugar left; desugar right ])
    | Ast.Unop(op, value) -> Expr.Call(op.handler, [ desugar value ])
    | Ast.If(branches, elseBranch) ->

        let ifCases =
            branches
            |> List.map (fun b -> {
                MatchCase.pattern = Pattern.Any
                body = desugar b.body
                guard = desugar b.cond |> Some
            })

        let elseCase = {
            pattern = Pattern.Any
            body =
                elseBranch
                |> Option.map desugar
                |> Option.defaultValue Expr.Nil
            guard = None
        }

        Expr.Match(Expr.Nil, ifCases @ [ elseCase ])

let rec optimize (expr: Expr) : Expr =
    match expr with
    | Expr.Call(name, args) ->
        let args = args |> List.map optimize

        match name, args with
        // Constants
        | "__neg", [ Expr.Num n ] -> Expr.Num -n
        | "__add", [ Expr.Num a; Expr.Num b ] -> Expr.Num(a + b)
        | "__sub", [ Expr.Num a; Expr.Num b ] -> Expr.Num(a - b)
        | "__mul", [ Expr.Num a; Expr.Num b ] -> Expr.Num(a * b)
        | "__div", [ Expr.Num a; Expr.Num b ] -> Expr.Num(a / b)
        | "__pow", [ Expr.Num a; Expr.Num b ] -> Expr.Num(a ** b)
        | "__mod", [ Expr.Num a; Expr.Num b ] -> Expr.Num(a % b)
        // Default case
        | _ -> Expr.Call(name, args)

    | Expr.List(items) ->
        items
        |> List.map (
            function
            | Elem e -> Elem(optimize e)
            | Splat splat -> Splat(optimize splat)
        )
        |> Expr.List
    | Expr.Table(items) ->
        items
        |> List.map (fun (k, v) -> optimize k, optimize v)
        |> Expr.Table
    | Expr.Let(name, value) -> Expr.Let(name, optimize value)
    | Expr.Assign(name, value) -> Expr.Assign(name, optimize value)
    | Expr.Match(value, cases) ->
        Expr.Match(
            optimize value,
            cases
            |> List.map (fun case -> {
                pattern = case.pattern
                guard = Option.map optimize case.guard
                body = optimize case.body
            })
        )
    | Expr.Closure(args, body) -> Expr.Closure(args, optimize body)
    | Expr.Block body ->
        match body with
        | [ x ] -> optimize x
        | _ -> Expr.Block(List.map optimize body)
    | _ -> expr
