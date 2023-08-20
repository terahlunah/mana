namespace Mana.Parser

open Mana

type Expr =
    | Unit
    | Bool of b: bool
    | Num of n: float
    | Char of c: char
    | Str of s: string
    | Ident of string
    | Call of name: string * args: Expr list
    | Closure of args: Argument list * body: Expr
    | Match of expr: Expr * cases: MatchCase list
    | Block of Expr list
    | List of Expr list
    | Table of (string * Expr) list
    | If of Expr * Expr * Expr
    | Let of name: string * expr: Expr

and Argument = Argument of string

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
