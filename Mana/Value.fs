namespace Mana

type Value =
    | Unit
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | List of items: List<Value>
    | Table of items: Map<string, Value>
    | Fun of handler: (Env<Value> -> List<Value> -> RuntimeResult<Env<Value> * Value>)

module Value =

    let isBool value =
        function
        | Bool b -> b = value
        | _ -> false

    let isNum value =
        function
        | Num n -> n = value
        | _ -> false

    let isStr value =
        function
        | Str s -> s = value
        | _ -> false
