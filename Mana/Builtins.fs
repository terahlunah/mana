module Mana.Builtins

open Mana
open Mana.Cont
open Mana.Error
open System

let discard _ = Value.Nil

// Env

let __env (env: Env<Value>) _ =
    let bindings =
        env.getAll ()
        |> Seq.map (fun kv -> (Value.Str kv.Key, kv.Value))
        |> Map.ofSeq
        |> Value.Table

    [
        (Value.Str "name", Value.Str env.name)
        (Value.Str "bindings", bindings)
    ]
    |> Map.ofList
    |> Value.Table

let __env_name (env: Env<Value>) _ = env.name |> Value.Str

// Coroutines and Channels

let co_spawn (ctx: Context<Value>) env args = cps {
    match args with
    | [ Closure handler ] ->
        let co () =
            // Discard coroutine return
            handler ctx env [] discard |> ignore
            // We need to run the remaining coroutines after this one finishes
            ctx.RunNextCoroutine()

        ctx.ScheduleCoroutine(co)
        return Value.Nil
    | _ -> return raiseError (ManaError.InvalidArguments "invalid `spawn` arguments")
}

let co_suspend (ctx: Context<Value>) env args (k: Value -> Value) =
    match args with
    | [] ->
        let co () = k Value.Nil
        ctx.ScheduleCoroutine(co)
        ctx.RunNextCoroutine()
    | _ -> raiseError (ManaError.InvalidArguments "invalid `suspend` arguments")

let ch_new (ctx: Context<Value>) env args = cps {
    match args with
    | [] -> return Channel.Create 0 |> Value.Channel
    | [ Num n ] -> return Channel.Create(int n) |> Value.Channel
    | _ -> return raiseError (ManaError.InvalidArguments "invalid `channel` arguments")
}

let ch_close (ctx: Context<Value>) env args = cps {
    match args with
    | [ Channel c ] ->
        ctx.CloseChannel c
        return Value.Nil
    | _ -> return raiseError (ManaError.InvalidArguments "invalid `close` arguments")
}

let ch_recv (ctx: Context<Value>) env args k =
    match args with
    | [ Channel c ] ->
        match c.SendDequeue(), c.BufDequeue() with
        | Some(s, sv), None ->
            ctx.ScheduleCoroutine(s)
            k sv
        | Some(s, sv), Some v ->
            c.BufEnqueue sv
            ctx.ScheduleCoroutine(s)
            k v
        | None, None ->
            c.RecvEnqueue k
            ctx.RunNextCoroutine()
        | None, Some v -> k v
    | _ -> raiseError (ManaError.InvalidArguments "invalid `recv` arguments")

let ch_send (ctx: Context<Value>) env args k =
    match args with
    | [ Channel c; v ] ->
        match c.RecvDequeue() with
        | Some r ->
            ctx.ScheduleCoroutine(fun _ -> r v)
            k Value.Nil
        | None when not (c.IsFull()) ->
            c.BufEnqueue v
            k Value.Nil
        | None ->
            c.SendEnqueue (fun _ -> k Value.Nil) v
            ctx.RunNextCoroutine()

    | _ -> raiseError (ManaError.InvalidArguments "invalid `send` arguments")

// Arithmetic
let neg env args =
    match args with
    | [ Num a ] -> -a |> Num
    | _ -> raiseError (ManaError.InvalidArguments "argument must be a number")

let add env args =
    match args with
    | [ Num a; Num b ] -> a + b |> Num
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let sub env args =
    match args with
    | [ Num a; Num b ] -> a - b |> Num
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let mul env args =
    match args with
    | [ Num a; Num b ] -> a * b |> Num
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let div env args =
    match args with
    | [ Num a; Num b ] -> a / b |> Num
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let pow env args =
    match args with
    | [ Num a; Num b ] -> a ** b |> Num
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let rem env args =
    match args with
    | [ Num a; Num b ] -> a % b |> Num
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

// Logic

let gt env args =
    match args with
    | [ Num a; Num b ] -> a > b |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let ge env args =
    match args with
    | [ Num a; Num b ] -> a >= b |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let lt env args =
    match args with
    | [ Num a; Num b ] -> a < b |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let le env args =
    match args with
    | [ Num a; Num b ] -> a <= b |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be numbers")

let eq env args =
    match args with
    | [ a; b ] -> a = b |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "expected 2 arguments")

let ne env args =
    match args with
    | [ a; b ] -> a <> b |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "expected 2 arguments")

let bnot env args =
    match args with
    | [ Bool a ] -> not a |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "argument must be a bool")

let band env args =
    match args with
    | [ Bool a; Bool b ] -> (a && b) |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be bool")

let bor env args =
    match args with
    | [ Bool a; Bool b ] -> (a || b) |> Bool
    | _ -> raiseError (ManaError.InvalidArguments "arguments must be bool")

// IO

let debug env args =
    let x = args |> List.map Value.repr |> String.concat ""
    printfn $"%A{x}"
    Value.Nil

let print env args =
    let x = args |> List.map Value.repr |> String.concat ""
    printfn $"%s{x}"
    Value.Nil

let rec cond ctx env args = cps {
    return!
        match args with
        | Closure condition :: Closure body :: rest -> cps {
            let! c = condition ctx env []

            let! r =
                if Value.isTrue c then
                    body ctx env []
                else
                    cond ctx env rest

            return r
          }
        | [] -> cps { return Value.Nil }
        | _ -> cps { return raiseError (ManaError.InvalidArguments "invalid `cond` arguments") }
}

let condIfElse ctx env args = cps {
    match args with
    | [ cond; Closure thenBody; Closure elseBody ] ->
        if Value.isTrue cond then
            return! thenBody ctx env []
        else
            return! elseBody ctx env []
    | _ -> return raiseError (ManaError.InvalidArguments "invalid `if` arguments")
}

let condWhen ctx env args = cps {
    match args with
    | [ cond; Closure body ] ->
        if Value.isTrue cond then
            return! body ctx env []
        else
            return Value.Nil
    | _ -> return raiseError (ManaError.InvalidArguments "invalid `when` arguments")
}

let nullCoalescing ctx env args = cps {
    match args with
    | [ cond; Closure body ] ->
        if Value.isTrue cond then
            return! body ctx env []
        else
            return Value.Nil
    | _ -> return raiseError (ManaError.InvalidArguments "invalid `when` arguments")
}

// Seq
let concat env args =
    match args with
    | [ Value.Str a; Value.Str b ] -> String.concat "" [ a; b ] |> Value.Str
    | [ Value.List a; Value.List b ] -> List.concat [ a; b ] |> Value.List
    | [ Value.Table a; Value.Table b ] ->
        Map.fold (fun acc key value -> Map.add key value acc) a b
        |> Value.Table
    | [ _ ] -> raiseError (ManaError.InvalidArguments $"both `concat` arguments must be lists, tables or strings")
    | _ -> raiseError (ManaError.InvalidArguments "`concat` takes 2 arguments")

let len env args =
    match args with
    | [ Value.Str x ] -> String.length x |> float |> Value.Num
    | [ Value.List x ] -> List.length x |> float |> Value.Num
    | [ Value.Table x ] -> Map.count x |> float |> Value.Num
    | [ v ] -> raiseError (ManaError.InvalidArguments $"`len` argument must be a list, a table or a string")
    | _ -> raiseError (ManaError.InvalidArguments "`len` takes 1 argument")

let head env args =
    match args with
    | [ Value.List x ] -> List.tryHead x |> Option.defaultValue Value.Nil
    | [ Value.Str x ] ->
        Seq.tryHead x
        |> Option.map (string >> Value.Str)
        |> Option.defaultValue Value.Nil
    | [ v ] -> raiseError (ManaError.InvalidArguments $"`head` argument must be a list or a string")
    | _ -> raiseError (ManaError.InvalidArguments "`head` takes 1 argument")

let tail env args =
    match args with
    | [ Value.List x ] -> List.tail x |> Value.List
    | [ Value.Str x ] -> Seq.tail x |> string |> Value.Str
    | [ _ ] -> raiseError (ManaError.InvalidArguments $"`tail` argument must be a list or a string")
    | _ -> raiseError (ManaError.InvalidArguments "`tail` takes 1 argument")

let rev env args =
    match args with
    | [ Value.List x ] -> List.rev x |> Value.List
    | [ Value.Str x ] -> Seq.rev x |> string |> Value.Str
    | [ _ ] -> raiseError (ManaError.InvalidArguments $"`rev` argument must be a list or a string")
    | _ -> raiseError (ManaError.InvalidArguments "`rev` takes 1 argument")

let get env args =
    match args with
    | [ Value.List x; Value.Num n ] ->
        let n = if n < 0 then x.Length - (int n) else (int n)
        List.tryItem (int n) x |> Option.defaultValue Value.Nil
    | [ Value.Str x; Value.Num n ] ->
        let n = if n < 0 then x.Length - (int n) else (int n)

        Seq.tryItem (int n) x
        |> Option.map (string >> Value.Str)
        |> Option.defaultValue Value.Nil
    | [ Value.Table x; key ] -> Map.tryFind key x |> Option.defaultValue Value.Nil
    | [ _; Value.Num _ ] ->
        raiseError (ManaError.InvalidArguments $"`get` first argument must be a list or a string when second is Num")
    | [ Value.Str _ | Value.List _; _ ] ->
        raiseError (ManaError.InvalidArguments $"`get` second argument must be a Num, when first is List or Str")
    | _ -> raiseError (ManaError.InvalidArguments "`get` takes 2 arguments")

let set env args =
    match args with
    | [ Value.List x; Value.Num index; value ] ->
        let n = if index < 0 then x.Length - (int index) else (int index)

        if n >= 0 && n < x.Length then
            x
            |> List.mapi (fun i v -> if i = n then value else v)
            |> Value.List
        else
            raiseError (ManaError.InvalidArguments "Index out of bounds")
    | [ Value.Table x; key; value ] -> Map.add key value x |> Value.Table
    | [ _; Value.Num _ ] ->
        raiseError (ManaError.InvalidArguments $"`set` first argument must be a list when second is Num")
    | [ Value.List _; _ ] ->
        raiseError (ManaError.InvalidArguments $"`set` second argument must be a Num when first is List")
    | _ -> raiseError (ManaError.InvalidArguments "`set` takes 2 arguments")

let contains env args =
    match args with
    | [ Value.List l; x ] -> l |> List.contains x |> Value.Bool
    | [ Value.Str s; Value.Str x ] -> s.Contains x |> Value.Bool
    | [ Value.Table t; key ] -> t |> Map.containsKey key |> Value.Bool
    | _ -> raiseError (ManaError.InvalidArguments "invalid arguments for `contains`")

// Predicates

let isNil env args =
    match args with
    | [ Nil ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raiseError (ManaError.InvalidArguments "`nil?` only takes 1 argument")

let isBool env args =
    match args with
    | [ Bool _ ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raiseError (ManaError.InvalidArguments "`bool?` only takes 1 argument")

let isNum env args =
    match args with
    | [ Num _ ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raiseError (ManaError.InvalidArguments "`num?` only takes 1 argument")

let isStr env args =
    match args with
    | [ Str _ ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raiseError (ManaError.InvalidArguments "`str?` only takes 1 argument")

let isList env args =
    match args with
    | [ List _ ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raiseError (ManaError.InvalidArguments "`list?` only takes 1 argument")

let isTable env args =
    match args with
    | [ Table _ ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raiseError (ManaError.InvalidArguments "`table?` only takes 1 argument")

let isFn env args =
    match args with
    | [ Closure _ ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raiseError (ManaError.InvalidArguments "`fn?` only takes 1 argument")

// Conversions

let toNum env args =
    match args with
    | [ Str s ] -> Num(float s)
    | [ Num n ] -> Num n
    | [ Bool b ] -> (if b then 1.0 else 0.0) |> Num
    | _ -> raiseError (ManaError.InvalidArguments "`num` only takes 1 argument")

let bind name f =
    let kf =
        fun ctx env args k ->
            // printfn $"debug call {name}"
            k (f env args)

    Env.set name (Value.Closure(kf))

let bindRaw name f = Env.set name (Value.Closure(f))

let env: Env<Value> =
    Env.empty ("builtins")

    // Meta
    |> bind "__env" __env

    // Coroutines
    |> bindRaw "spawn" co_spawn
    |> bindRaw "suspend" co_suspend

    // Channels
    |> bindRaw "channel" ch_new
    |> bindRaw "close" ch_close
    |> bindRaw "send" ch_send
    |> bindRaw "recv" ch_recv

    // Arithmetic
    |> Env.set "pi" (Value.Num Math.PI)
    |> bind "__add" add
    |> bind "__neg" neg
    |> bind "__sub" sub
    |> bind "__mul" mul
    |> bind "__div" div
    |> bind "__pow" pow
    |> bind "__mod" rem

    // Logic
    |> bind "__gt" gt
    |> bind "__ge" ge
    |> bind "__lt" lt
    |> bind "__le" le
    |> bind "__eq" eq
    |> bind "__ne" ne
    |> bind "__not" bnot
    |> bind "__and" band
    |> bind "__or" bor

    // IO
    |> bind "print" print
    |> bind "debug" debug

    // Control Flow
    |> bindRaw "cond" cond
    |> bindRaw "if" condIfElse
    |> bindRaw "when" condWhen
    |> bindRaw "__null_coalescing" nullCoalescing

    // Seq
    |> bind "concat" concat
    |> bind "head" head
    |> bind "tail" tail
    |> bind "len" len
    |> bind "rev" rev
    |> bind "get" get
    |> bind "set" set
    |> bind "contains" contains

    // Predicates
    |> bind "is_nil" isNil
    |> bind "is_bool" isBool
    |> bind "is_num" isNum
    |> bind "is_str" isStr
    |> bind "is_list" isList
    |> bind "is_table" isTable
    |> bind "is_fn" isFn

    // Conversions
    |> bind "num" toNum
