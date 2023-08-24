namespace Mana.Parser

open FParsec
open Mana

type Expr =
    | Unit
    | Bool of b: bool
    | Num of n: float
    | Char of c: char
    | Str of s: string
    | Ident of string
    | Call of symbol: string * args: Expr list
    | BinaryOp of op: BinaryOperator * left: Expr * right: Expr
    | UnaryOp of op: UnaryOperator * arg: Expr
    | Closure of args: Argument list * body: Expr
    | Match of expr: Expr * cases: MatchCase list
    | Block of Expr list
    | List of Expr list
    | Table of (string * Expr) list
    | If of Expr * Expr * Expr
    | Let of name: string * expr: Expr

and Argument =
    | Named of string
    | Unit

and MatchCase = {
    pattern: Pattern
    body: Expr
}

and Pattern =
    | BoolPattern of b: bool
    | NumPattern of n: float
    | StrPattern of s: string
    | IdentPattern of i: string
    | ListPattern of Pattern list
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

// type UseStatement = { name: string }

type Program = { modules: Module list }
