namespace Mana.Parser

type ParseErrorKind =
    | UnexpectedEof
    | UnexpectedChar of char
    | ExpectedDigit
    | ParseNum of string
    | BadIndentation
    | ExpectedToken of expected: Token * got: Token
    | ExpectedDefinition of got: Token
    | ExpectedLiteral of got: Token
    | ExpectedExpr of got: Token

type ParseError = {
    kind: ParseErrorKind
    span: Span
}

type ParseResult<'T> = Result<'T, ParseError>
