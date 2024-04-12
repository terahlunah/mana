namespace Mana

type Pattern =
    | Nil
    | Bool of b: bool
    | Num of n: float
    | Str of s: string
    | Symbol of s: string
    | List of ListPatternItem list
    | Table of TablePatternItem list
    | Wildcard

and CollectionPatternItem<'T> =
    | Single of item: 'T
    | Rest of name: string

and ListPatternItem = CollectionPatternItem<Pattern>

and TablePatternItem = CollectionPatternItem<Pattern * Pattern>

module Pattern =

    let isSingle =
        function
        | Single _ -> true
        | Rest _ -> false

    let isRest =
        function
        | Single _ -> false
        | Rest _ -> true

    let isCollectionPatternVariable (p: CollectionPatternItem<_> list) =
        let numberOfRest = p |> List.filter isRest |> List.length

        numberOfRest > 0

    let isCollectionPatternValid (p: CollectionPatternItem<_> list) =
        let numberOfRest = p |> List.filter isRest |> List.length

        numberOfRest <= 1

    let minimumSize (p: CollectionPatternItem<_> list) =
        p |> List.filter isSingle |> List.length

    let maximumSize (p: CollectionPatternItem<_> list) =
        if p |> List.exists isRest then
            System.Int32.MaxValue
        else
            p |> List.length

    let matchesSize n (p: CollectionPatternItem<_> list) =
        n >= minimumSize p && n <= maximumSize p
