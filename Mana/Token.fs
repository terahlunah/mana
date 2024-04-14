namespace Mana

type TokenKind =
    | LParen
    | RParen
    | LBracket
    | RBracket
    | LBrace
    | RBrace
    | Hash
    | Nil
    | Bool
    | Num
    | Str
    | Symbol
    | Comma
    | Colon
    | Let
    | Match
    | Eq
    | Pipe
    | Operator
    | Wildcard
    | Rest
    | Arrow
    | NewLine
    | Dot
    | Eof

type TokenData =
    | Bool of b: bool
    | Num of n: double
    | Str of s: string

type Token = {
    kind: TokenKind
    span: Span
    data: TokenData option
} with

    override self.ToString() =
        $"{self.kind}({self.data}) @ {self.span}"

module Token =
    let make token = {
        kind = token
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
    let withStr data ts = { ts with data = Some(Str data) }

    let asBool ts =
        match ts.data with
        | Some(Bool b) -> Some b
        | _ -> None

    let asNum ts =
        match ts.data with
        | Some(Num b) -> Some b
        | _ -> None

    let asStr ts =
        match ts.data with
        | Some(Str b) -> Some b
        | _ -> None
