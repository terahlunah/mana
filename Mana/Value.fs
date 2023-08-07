namespace Mana

type Value =
    | Unit
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | List of items: List<Value>
    | Table of items: Map<string, Value>
    | Fun of handler: (Env<Value> -> List<Value> -> RuntimeResult<Env<Value> * Value>)
