namespace Mana

type Value =
    | Unit
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | Fun of f: string
    | Native of handler: (Value list -> RuntimeResult<Value>)
