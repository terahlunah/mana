namespace Mana.Parser

type Token =
    | Indent
    | Dedent
    | LParen
    | RParen
    | LBracket
    | RBracket
    | LBrace
    | RBrace
    | Bool
    | Num
    | Char
    | Str
    | Def
    | Eq
    | Arrow
    | Comma
    | Colon
    | Mod
    | If
    | Then
    | Else
    | Match
    | With
    | Term
    | NewLine
    | Eof

module Token =
    let isExpr =
        function
        | LParen
        | LBrace
        | LBracket
        | Bool
        | Num
        | Char
        | Str
        | Term
        | If
        | Match -> true
        | _ -> false

type TokenData =
    | Bool of b: bool
    | Num of n: double
    | Char of c: char
    | Str of s: string

type TokenSpan = {
    token: Token
    span: Span
    data: TokenData option
}

module TokenSpan =
    let ofToken token = {
        token = token
        span = {
            source = ""
            start = 0
            size = 0
        }
        data = None
    }

    let withSpan span ts = { ts with span = span }
    let withData data ts = { ts with data = Some data }
    let withBool data ts = { ts with data = Some(Bool data) }
    let withNum data ts = { ts with data = Some(Num data) }
    let withChar data ts = { ts with data = Some(Char data) }
    let withStr data ts = { ts with data = Some(Str data) }

    let asBool ts =
        match ts.data with
        | Some(Bool b) -> Some b
        | _ -> None

    let asNum ts =
        match ts.data with
        | Some(Num b) -> Some b
        | _ -> None

    let asChar ts =
        match ts.data with
        | Some(Char b) -> Some b
        | _ -> None

    let asStr ts =
        match ts.data with
        | Some(Str b) -> Some b
        | _ -> None
