module Mana.Core

open Mana
open Mana.Error
open Yute

let debug env args =
    args |> List.map Value.repr |> String.concat "" |> debug
    Value.Nil

let display env args =
    args |> List.map Value.repr |> String.concat "" |> display
    Value.Nil

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

let env: Env<Value> =
    Env.empty ()
    |> Env.set "__add" (Value.Closure add)
    |> Env.set "__neg" (Value.Closure neg)
    |> Env.set "__sub" (Value.Closure sub)
    |> Env.set "__mul" (Value.Closure mul)
    |> Env.set "__div" (Value.Closure div)
    |> Env.set "__pow" (Value.Closure pow)
    |> Env.set "__mod" (Value.Closure rem)
    |> Env.set "__gt" (Value.Closure gt)
    |> Env.set "__ge" (Value.Closure ge)
    |> Env.set "__lt" (Value.Closure lt)
    |> Env.set "__le" (Value.Closure le)
    |> Env.set "__eq" (Value.Closure eq)
    |> Env.set "__ne" (Value.Closure ne)
    |> Env.set "__not" (Value.Closure bnot)
    |> Env.set "__and" (Value.Closure band)
    |> Env.set "__or" (Value.Closure div)
    |> Env.set "__print" (Value.Closure display)
    |> Env.set "__debug" (Value.Closure debug)
