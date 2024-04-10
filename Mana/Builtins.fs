module Mana.Builtins

open FsToolkit.ErrorHandling
open Mana
open Mana.Error
open Yute
open System

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
    args |> List.map Value.repr |> String.concat "" |> debug
    Value.Nil

let display env args =
    args |> List.map Value.repr |> String.concat "" |> display
    Value.Nil

let rec cond env args =
    match args with
    | Closure condition :: Closure body :: rest ->
        let c = condition env []
        if Value.isTrue c then body env [] else cond env rest
    | [] -> Value.Nil
    | _ -> raiseError (ManaError.InvalidArguments "invalid `cond` arguments")

let condIfElse env args =
    match args with
    | [ cond; Closure thenBody; Closure elseBody ] ->
        if Value.isTrue cond then
            thenBody env []
        else
            elseBody env []
    | _ -> raiseError (ManaError.InvalidArguments "invalid `if` arguments")

let condWhen env args =
    match args with
    | [ cond; Closure body ] -> if Value.isTrue cond then body env [] else Value.Nil
    | _ -> raiseError (ManaError.InvalidArguments "invalid `when` arguments")

// Seq
let concat env args =
    match args with
    | [ Value.Str a; Value.Str b ] -> String.concat "" [ a; b ] |> Value.Str
    | [ Value.List a; Value.List b ] -> List.concat [ a; b ] |> Value.List
    | [ Value.Table a; Value.Table b ] -> Map.merge a b |> Value.Table
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
    | [ Value.Str x ] -> Seq.tail x |> String.Concat |> Value.Str
    | [ v ] -> raiseError (ManaError.InvalidArguments $"`tail` argument must be a list or a string")
    | _ -> raiseError (ManaError.InvalidArguments "`tail` takes 1 argument")

// let nth env args =
//     match args with
//     | [ Value.List x; Value.Num n ] -> List.tryItem (int n) x |> Option.defaultValue Value.Nil
//     | [ Value.Str x; Value.Num n ] ->
//         Seq.tryItem (int n) x
//         |> Option.map (string >> Value.Str)
//         |> Option.defaultValue Value.Nil
//     | [ _; Value.Num _ ] -> raiseError (ManaError.InvalidArguments $"`nth` first argument must be a list or a string")
//     | [ Value.Str _ | Value.List _; _ ] ->
//         raiseError (ManaError.InvalidArguments $"`nth` second argument must be a number")
//     | _ -> raiseError (ManaError.InvalidArguments "`nth` takes 2 arguments")

//

let env: Env<Value> =
    Env.empty ()
    // Arithmetic
    |> Env.set "pi" (Value.Num Math.PI)
    |> Env.set "__add" (Value.Closure add)
    |> Env.set "__neg" (Value.Closure neg)
    |> Env.set "__sub" (Value.Closure sub)
    |> Env.set "__mul" (Value.Closure mul)
    |> Env.set "__div" (Value.Closure div)
    |> Env.set "__pow" (Value.Closure pow)
    |> Env.set "__mod" (Value.Closure rem)

    // Logic
    |> Env.set "__gt" (Value.Closure gt)
    |> Env.set "__ge" (Value.Closure ge)
    |> Env.set "__lt" (Value.Closure lt)
    |> Env.set "__le" (Value.Closure le)
    |> Env.set "__eq" (Value.Closure eq)
    |> Env.set "__ne" (Value.Closure ne)
    |> Env.set "__not" (Value.Closure bnot)
    |> Env.set "__and" (Value.Closure band)
    |> Env.set "__or" (Value.Closure bor)

    // IO
    |> Env.set "display" (Value.Closure display)
    |> Env.set "debug" (Value.Closure debug)

    // Control Flow
    |> Env.set "cond" (Value.Closure cond)
    |> Env.set "if" (Value.Closure condIfElse)
    |> Env.set "when" (Value.Closure condWhen)

    // Seq
    |> Env.set "head" (Value.Closure head)
    |> Env.set "tail" (Value.Closure tail)
    |> Env.set "len" (Value.Closure len)
