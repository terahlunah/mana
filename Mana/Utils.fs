[<AutoOpen>]
module Mana.Utils

let inline todo msg = failwith $"TODO: %s{msg}"

module String =
    let at (index: int) (str: string) =
        if index >= 0 && index < String.length str then
            Some str[index]
        else
            None

module Option =

    let inline orRaise error o =
        match o with
        | Some t -> t
        | None -> error |> raise
