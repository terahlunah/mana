namespace Mana

[<AutoOpen>]
module Core =
    let sdebug x = sprintf $"%A{x}"
    let debug x = printfn $"%A{x}"
    let display x = printfn $"%s{x}"

module Option =
    let okOr err o =
        match o with
        | Some x -> Ok x
        | None -> Error err

module Result =

    let unwrap r =
        match r with
        | Ok t -> t
        | Error e -> e |> sdebug |> failwith

    let any r = Result.mapError (fun x -> x :> obj) r

    let anyBind f r =
        match r with
        | Ok x -> f x |> Result.mapError (fun x -> x :> obj)
        | Error e -> e :> obj |> Result.Error

module Tuple3 =
    let map f (a, b, c) = (f a, f b, f c)

module Map =
    let merge m1 m2 =
        Map.fold (fun acc key value -> Map.add key value acc) m1 m2

module List =
    let rec foldResult<'T, 'State, 'E> (folder: 'State -> 'T -> Result<'State, 'E>) (state: 'State) (list: 'T list) =
        match list with
        | [] -> Ok state
        | head :: tail ->
            match folder state head with
            | Ok state -> foldResult folder state tail
            | Error e -> Error e
