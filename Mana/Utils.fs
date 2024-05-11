[<AutoOpen>]
module Mana.Utils

open System.Collections.Generic

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

module Queue =

    let tryDequeue (queue: Queue<'T>) : 'T option =
        match queue.TryDequeue() with
        | true, v -> Some v
        | _ -> None
