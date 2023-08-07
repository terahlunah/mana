module Mana.Ast

open Mana

type Expr =
    | Unit
    | NumberLiteral of float
    | StringLiteral of string
    | Ident of string
    | Call of name: string * args: Expr list
    | Lambda of Argument list * Expr
    | Match of Expr * MatchCase list
    | Block of Expr list
    | List of Pattern list
    | Dict of (string * Pattern) list
    | If of Expr * Expr * Expr
    | Let of string * Expr

and Argument = string

and MatchCase = {
    pattern: Pattern
    body: Expr
}

and Pattern =
    | NumberPattern of float
    | StringPattern of string
    | IdentPattern of string
    // | ListPattern of string
    | Underscore

type Module = {
    name: string
    definitions: Definition list
}

and Definition = {
    name: string
    args: Argument list
    body: Expr
}

and NativeFunction = {
    name: string
    arity: int
    handler: Env<Value> -> List<Value> -> Env<Value> * RuntimeResult<Value>
}

type UseStatement = { name: string }

type Program = { modules: Module list }
