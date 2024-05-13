namespace Mana

type Expr =
    | Nil
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | List of items: ListItem<Expr> list
    | Table of items: (Expr * Expr) list
    | Block of body: Expr list
    | Call of name: string * args: Expr list
    | Closure of args: string list * body: Expr
    | Assign of symbol: string * value: Expr
    | Let of pattern: Pattern * value: Expr
    | Match of expr: Expr * cases: MatchCase<Expr> list
