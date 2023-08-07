module Mana.Std.Core

open Mana

let add env args =
    match args with
    | [ Num a; Num b ] -> Ok(env, a + b |> Num)
    | _ -> Error RuntimeError.InvalidArguments
