module Tests.Console

open System

type Colored(color: ConsoleColor) =
    member _.Zero() = ()

    member _.Delay f = f

    member _.Run f =
        let current = Console.ForegroundColor
        Console.ForegroundColor <- color
        f ()
        Console.ForegroundColor <- current

let inline colored color = Colored color
