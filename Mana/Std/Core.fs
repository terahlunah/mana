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
