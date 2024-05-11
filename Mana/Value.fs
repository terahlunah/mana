namespace Mana

open System
open Mana.Error
open Mana.Cont

[<CustomComparison; CustomEquality>]
type Value =
    | Nil
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | List of items: Value list
    | Table of items: Map<Value, Value>
    | Closure of handler: (Context<Value> -> Env<Value> -> List<Value> -> (Value -> Value) -> Value)
    | Channel of Channel<Value>

    [<CompiledName("AsBool")>]
    member this.asBool =
        match this with
        | Bool b -> b
        | _ -> failwith "Invalid Value access"

    [<CompiledName("AsNum")>]
    member this.asNum =
        match this with
        | Num n -> n
        | _ -> failwith "Invalid Value access"

    [<CompiledName("AsStr")>]
    member this.asStr =
        match this with
        | Str s -> s
        | _ -> failwith "Invalid Value access"

    [<CompiledName("AsList")>]
    member this.asList =
        match this with
        | List l -> l
        | _ -> failwith "Invalid Value access"

    [<CompiledName("AsTable")>]
    member this.asTable =
        match this with
        | Table t -> t
        | _ -> failwith "Invalid Value access"

    static member NewFuncClosure(handler: System.Func<Env<Value>, ResizeArray<Value>, Value>) =
        let h (ctx: Context<Value>) (env: Env<Value>) (args: List<Value>) = cps {
            return handler.Invoke(env, ResizeArray args)
        }

        Value.Closure h

    member this.Rank() =
        match this with
        | Nil -> 0
        | Bool _ -> 1
        | Num _ -> 2
        | Str _ -> 3
        | List _ -> 4
        | Table _ -> 5
        | Closure _ -> 6
        | Channel _ -> 7

    interface IEquatable<Value> with
        member this.Equals other =
            match this, other with
            | Nil, Nil -> true
            | Bool a, Bool b -> a = b
            | Num a, Num b -> a = b
            | Str a, Str b -> a = b
            | List a, List b -> a = b
            | Table a, Table b -> a = b
            | Closure _, Closure _ -> false
            | Channel _, Channel _ -> false
            | _ -> false

    override this.Equals other =
        match other with
        | :? Value as o -> (this :> IEquatable<_>).Equals o
        | _ -> false

    interface IComparable<Value> with
        member this.CompareTo other =
            match this, other with
            | Nil, Nil -> 0
            | Bool a, Bool b -> compare a b
            | Num a, Num b -> compare a b
            | Str a, Str b -> compare a b
            | List a, List b -> compare a b
            | Table a, Table b -> compare a b
            | Closure _, Closure _ -> 0
            | Channel _, Channel _ -> 0
            | _ -> compare (this.Rank()) (other.Rank())

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Value as o -> (this :> IComparable<_>).CompareTo o
            | _ -> -1

    override this.GetHashCode() =
        match this with
        | Nil -> 0
        | Bool true -> 1
        | Bool false -> 2
        | Num n -> hash n
        | Str s -> hash s
        | List items -> hash items
        | Table items -> hash items
        | Closure _ -> raiseError ClosureCantBeUsedAsKey
        | Channel _ -> raiseError ChannelCantBeUsedAsKey

module Value =
    let isTrue =
        function
        | Bool false
        | Nil -> false
        | _ -> true

    let isFalse =
        function
        | Bool false
        | Nil -> true
        | _ -> false

    let isNil =
        function
        | Nil -> true
        | _ -> false

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

    let rec repr (v: Value) =
        match v with
        | Nil -> "nil"
        | Bool b -> $"%b{b}"
        | Num n -> $"%g{n}"
        | Str s -> $"\"%s{s}\""
        | Closure _ -> "<closure>"
        | Channel _ -> "<channel>"
        | List items ->
            items
            |> Seq.map repr
            |> String.concat ", "
            |> sprintf "[%s]"
        | Table(items) ->
            items
            |> Seq.map (fun kv -> $"%s{repr kv.Key}: %s{repr kv.Value}")
            |> String.concat ", "
            |> sprintf "#[%s]"
