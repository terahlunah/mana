namespace Mana

type Value =
    | Unit
    | Bool of b: bool
    | Num of n: float
    | Char of c: char
    | Str of s: string
    | List of items: List<Value>
    | Table of items: Map<string, Value>
    | Fun of handler: (Env<Value> -> List<Value> -> RuntimeResult<Env<Value> * Value>)

module Value =

    let rec toString (value: Value) =
        match value with
        | Unit -> "()"
        | Bool b -> $"%b{b}"
        | Num n -> $"%f{n}"
        | Char c -> $"%c{c}"
        | Str s -> s
        | List items ->
            items
            |> List.map toString
            |> String.concat ", "
            |> sprintf "[%s]"
        | Table items ->
            items
            |> Map.toList
            |> List.map (fun (k, v) -> $"%s{k}: %s{toString v}")
            |> String.concat ", "
            |> sprintf "[%s]"
        | Fun _ -> "fun"

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
