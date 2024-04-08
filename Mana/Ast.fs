namespace Mana

type Ast =
    | Nil
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | List of items: Ast list
    | Table of items: (Ast * Ast) list
    | Let of name: string * value: Ast
    | Call of name: string * args: Ast list
    | Closure of args: string list * body: Ast list
