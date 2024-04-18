module Mana.Error

type ManaError =
    | UnexpectedEof
    | UnexpectedChar of char
    | ExpectedChar of char
    | ExpectedDigit
    | ParseNum of string
    | ExpectedToken of expected: TokenKind * got: TokenKind
    | ExpectedDefinition of got: TokenKind
    | ExpectedLiteral of got: TokenKind
    | ExpectedExpr of got: TokenKind
    | ExpectedSymbol of got: TokenKind
    | InvalidArguments of string
    | SymbolNotFound of name: string
    | UnknownOperator of name: string
    | ExpectedOperator
    | InvalidArgumentCount of f: string * got: int * expected: int
    | UnknownSymbol of name: string
    | FunctionNotFound of name: string
    | NotAFunction of value: string
    | LetBinding of string
    | PatternMatchingFailed
    | ClosureCantBeUsedAsKey
    | MoreThanOneRestPattern
    | OnlyListsCanBeSplatted

type ManaResult<'T> = Result<'T, ManaError>

exception ManaException of error: ManaError

let raiseError e = ManaException e |> raise
