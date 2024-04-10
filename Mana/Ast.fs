namespace Mana

open Yute

type Ast =
    | Nil
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | List of items: Ast list
    | Table of items: (Ast * Ast) list
    | Call of name: string * args: Ast list
    | Closure of args: string list * body: Ast list
    | Let of name: string * value: Ast
    // | Let of pattern: Pattern * value: Ast
    // | Match of expr: Expr * cases: MatchCase list

and MatchCase = {
    pattern: Pattern
    body: Ast
}

and Pattern =
    | Nil
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | Symbol of s: string
    | List of Pattern list
    | Table of Pattern list
    | Underscore

module Ast =
    let rec useImplicitIt (exprs: Ast list) : bool =
        match exprs with
        | [] -> false
        | head :: tail ->
            let useIt =
                match head with
                | Ast.List items -> useImplicitIt items
                | Ast.Table items ->
                    let ks = items |> List.map fst |> useImplicitIt
                    let vs = items |> List.map snd |> useImplicitIt
                    ks || vs
                | Ast.Let("it", _) -> false
                | Ast.Let(_, value) -> useImplicitIt [ value ]
                | Ast.Call("it", _) -> true
                | Ast.Call(_, args) -> useImplicitIt args
                | Ast.Closure(args, body) ->
                    useImplicitIt body
                    && args <> List.empty
                    && not ("it" =? args)
                | _ -> false

            useIt || useImplicitIt tail

    let rec optimizeAndDesugar (ast: Ast) : Ast =
        match ast with
        | Ast.Call(name, args) ->
            let args = args |> List.map optimizeAndDesugar

            match name, args with
            // Constants
            | "__neg", [ Ast.Num n ] -> Ast.Num -n
            | "__add", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a + b)
            | "__sub", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a - b)
            | "__mul", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a * b)
            | "__div", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a / b)
            | "__pow", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a ** b)
            | "__mod", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a % b)

            // Desugaring __chain virtual method
            | "__chain", [ x; Ast.Call(name, args) ] ->
                Ast.Call(name, optimizeAndDesugar x :: List.map optimizeAndDesugar args)
            | _ -> Ast.Call(name, args)

        | Ast.List(items) -> items |> List.map optimizeAndDesugar |> Ast.List
        | Ast.Table(items) ->
            items
            |> List.map (fun (k, v) -> optimizeAndDesugar k, optimizeAndDesugar v)
            |> Ast.Table
        | Ast.Let(name, value) -> Ast.Let(name, optimizeAndDesugar value)
        | Ast.Closure(args, body) -> Ast.Closure(args, List.map optimizeAndDesugar body)
        | _ -> ast
