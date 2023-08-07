namespace Mana

[<AutoOpen>]
module Core =
    let debug x = printfn $"%A{x}"

module Option =
    let okOr err o =
        match o with
        | Some x -> Ok x
        | None -> Error err

module Result =

    let any r = Result.mapError (fun x -> x :> obj) r

    let anyBind f r =
        match r with
        | Ok x -> f x |> Result.mapError (fun x -> x :> obj)
        | Error e -> e :> obj |> Result.Error

module Tuple3 =
    let map f (a, b, c) = (f a, f b, f c)
