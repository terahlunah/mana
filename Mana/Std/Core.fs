module Mana.Std.Core

open Mana

let add args =
    match args with
    | [ Num a; Num b ] -> a + b |> Num |> Ok
    | _ -> Error RuntimeError.InvalidArguments
