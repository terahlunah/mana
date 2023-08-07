namespace Mana

module Option =
    let okOr err o =
        match o with
        | Some x -> Ok x
        | None -> Error err
        
module Tuple3 =
    let map f (a, b, c) = (f a, f b, f c)