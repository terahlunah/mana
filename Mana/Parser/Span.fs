namespace Mana.Parser

type Span = {
    source: string
    start: int
    size: int
}

module Span =

    let zero = {
        source = ""
        start = 0
        size = 0
    }

    let display span =
        let src =
            span.source
            |> String.toList
            |> List.map (fun c ->
                match c with
                | '\n' -> "↵"
                | c -> string c
            )
            |> String.concat ""

        printfn $"%s{src}"
        printfn "%s↑" (String.replicate span.start " ")
