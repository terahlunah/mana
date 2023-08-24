module Mana.Std.Core

open Mana

let debug env args =
    args |> List.map Value.toString |> String.concat "" |> debug
    Ok(env, Value.Unit)

let display env args =
    args
    |> List.map Value.toString
    |> String.concat ""
    |> display

    Ok(env, Value.Unit)

let add env args =
    match args with
    | [ Num a; Num b ] -> Ok(env, a + b |> Num)
    | _ -> Error RuntimeError.InvalidArguments

let sub env args =
    match args with
    | [ Num a; Num b ] -> Ok(env, a - b |> Num)
    | _ -> Error RuntimeError.InvalidArguments

let neg env args =
    match args with
    | [ Num a ] -> Ok(env, -a |> Num)
    | _ -> Error RuntimeError.InvalidArguments

let mul env args =
    match args with
    | [ Num a; Num b ] -> Ok(env, a * b |> Num)
    | _ -> Error RuntimeError.InvalidArguments

let div env args =
    match args with
    | [ Num a; Num b ] -> Ok(env, a / b |> Num)
    | _ -> Error RuntimeError.InvalidArguments
