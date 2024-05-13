namespace Mana

type Ast =
    | Nil
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | List of items: ListItem<Ast> list
    | Table of items: (Ast * Ast) list
    | Block of body: Ast list
    | Call of name: string * args: Ast list
    | Closure of args: string list * body: Ast
    | Assign of symbol: string * value: Ast
    | Let of binding: Pattern * value: Ast
    | Match of expr: Ast * cases: MatchCase<Ast> list
    // Syntax Sugar
    | Binop of op: BinaryOperator * left: Ast * right: Ast
    | Unop of op: UnaryOperator * value: Ast
    | If of branches: IfBranch list * elseBranch: Ast option
// | ForIn of binding: Pattern * seq: Ast * body: Ast
// | While of cond: Ast * body: Ast

and IfBranch = {
    cond: Ast
    body: Ast
}

module Ast =
    let rec useImplicitIt (exprs: Ast list) : bool =
        match exprs with
        | [] -> false
        | head :: tail ->
            let useIt =
                match head with
                | Ast.Nil
                | Ast.Bool _
                | Ast.Num _
                | Ast.Str _ -> false
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
                | Ast.Block body -> useImplicitIt body
                | Ast.Assign(_, value) -> useImplicitIt [ value ]
                | Ast.Match(expr, cases) ->
                    useImplicitIt [ expr ]
                    || Seq.exists
                        (fun (case: MatchCase<_>) ->
                            useImplicitIt (
                                case.body
                                :: (case.guard
                                    |> Option.map List.singleton
                                    |> Option.defaultValue [])
                            )
                        )
                        cases
                | Binop(op, left, right) -> useImplicitIt [ left; right ]
                | Unop(op, value) -> useImplicitIt [ value ]
                | If(branches, elseBranch) ->
                    elseBranch |> Option.map List.singleton |> Option.map useImplicitIt |> Option.defaultValue false
                    || branches |> List.map _.cond |> useImplicitIt
                    || branches |> List.map _.body |> useImplicitIt

            useIt || useImplicitIt tail
    