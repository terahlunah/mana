namespace Mana

type Ast =
    | Nil
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | List of items: ListExpr<Ast> list
    | Table of items: (Ast * Ast) list
    | Block of body: Ast list
    | Call of name: string * args: Ast list
    | Closure of args: string list * body: Ast
    | Assign of symbol: string * value: Ast
    | Let of pattern: Pattern * value: Ast
    | Match of expr: Ast * cases: MatchCase list

and ListExpr<'T> =
    | Elem of elem: 'T
    | Splat of splat: 'T

and MatchCase = {
    pattern: Pattern
    guard: Ast option
    body: Ast
}

module Ast =
    let rec useImplicitIt (exprs: Ast list) : bool =
        match exprs with
        | [] -> false
        | head :: tail ->
            let useIt =
                match head with
                | Nil
                | Bool _
                | Num _
                | Str _ -> false
                | Ast.List items ->
                    items
                    |> List.map (
                        function
                        | Elem e -> e
                        | Splat splat -> splat
                    )
                    |> useImplicitIt
                | Ast.Table items ->
                    let ks = items |> List.map fst |> useImplicitIt
                    let vs = items |> List.map snd |> useImplicitIt
                    ks || vs
                | Ast.Let(_, value) -> useImplicitIt [ value ]
                | Ast.Call("it", _) -> true
                | Ast.Call(_, args) -> useImplicitIt args
                | Ast.Closure(args, body) ->
                    useImplicitIt [ body ]
                    && args <> List.empty
                    && not (args |> Seq.exists ((=) "it"))
                | Block body -> useImplicitIt body
                | Assign(_, value) -> useImplicitIt [ value ]
                | Match(expr, cases) ->
                    useImplicitIt [ expr ]
                    || Seq.exists
                        (fun case ->
                            useImplicitIt (
                                case.body
                                :: (case.guard
                                    |> Option.map List.singleton
                                    |> Option.defaultValue [])
                            )
                        )
                        cases

            useIt || useImplicitIt tail

    let rec optimize (ast: Ast) : Ast =
        match ast with
        | Ast.Call(name, args) ->
            let args = args |> List.map optimize

            match name, args with
            // Constants
            | "__neg", [ Ast.Num n ] -> Ast.Num -n
            | "__add", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a + b)
            | "__sub", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a - b)
            | "__mul", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a * b)
            | "__div", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a / b)
            | "__pow", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a ** b)
            | "__mod", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a % b)
            // Default case
            | _ -> Ast.Call(name, args)

        | Ast.List(items) ->
            items
            |> List.map (
                function
                | Elem e -> Elem(optimize e)
                | Splat splat -> Splat(optimize splat)
            )
            |> Ast.List
        | Ast.Table(items) ->
            items
            |> List.map (fun (k, v) -> optimize k, optimize v)
            |> Ast.Table
        | Ast.Let(name, value) -> Ast.Let(name, optimize value)
        | Ast.Assign(name, value) -> Ast.Assign(name, optimize value)
        | Ast.Match(value, cases) ->
            Ast.Match(
                optimize value,
                cases
                |> List.map (fun case -> {
                    case with
                        guard = Option.map optimize case.guard
                        body = optimize case.body
                })
            )
        | Ast.Closure(args, body) -> Ast.Closure(args, optimize body)
        | Ast.Block body ->
            match body with
            | [ x ] -> optimize x
            | _ -> Ast.Block(List.map optimize body)
        | _ -> ast
