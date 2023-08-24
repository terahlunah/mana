namespace Mana

open FsToolkit.ErrorHandling

[<AutoOpen>]
module Core =
    let inline sdebug x = sprintf $"%A{x}"
    let inline debug x = printfn $"%A{x}"
    let inline display x = printfn $"%s{x}"

    let inline todo msg = failwith msg

    let inline thenSome s b =
        match b with
        | true -> Some s
        | false -> None

    let inline thenOkOr o e b =
        match b with
        | true -> Ok o
        | false -> Error e

    let inline takeIf f x = f x |> thenSome x

    let inline parseFloat s =
        try
            Some(float s)
        with _ ->
            None

    let inline contains x s = Seq.exists ((=) x) s
    let inline (=?) x s = Seq.exists ((=) x) s

    let inline whileSome cond f =
        let rec loop () =
            match cond () with
            | Some x ->
                f x
                loop ()
            | None -> ()

        loop ()

module Option =
    let okOr err o =
        match o with
        | Some x -> Ok x
        | None -> Error err

    let inline unwrap o =
        match o with
        | Some t -> t
        | None -> "unwrapped a None Option" |> failwith

module Result =

    let inline unwrap r =
        match r with
        | Ok t -> t
        | Error e -> e |> sprintf "unwrapped a Error Result: %A" |> failwith

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

module String =
    let at (index: int) (str: string) =
        if index >= 0 && index < String.length str then
            Some str[index]
        else
            None

    let toList = Seq.toList
