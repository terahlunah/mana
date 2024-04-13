namespace Mana

type Span = {
    source: string
    start: int
    size: int
} with

    override self.ToString() =
        $"%A{self.start} -> %A{self.start + self.size - 1}"

    member self.pretty() =
        let src =
            self.source
            |> Seq.toList
            |> List.map (fun c ->
                match c with
                | '\r' -> ""
                | '\n' -> "→"
                | c -> string c
            )
            |> String.concat ""

        let arrow = String.replicate self.start " "

        sprintf $"%s{src}\n%s{arrow}↑\n"

module Span =

    let zero = {
        source = ""
        start = 0
        size = 0
    }

    let display span =
        let src =
            span.source
            |> Seq.toList
            |> List.map (fun c ->
                match c with
                | '\n' -> "↵"
                | c -> string c
            )
            |> String.concat ""

        printfn $"%s{src}"
        printfn "%s↑" (String.replicate span.start " ")
