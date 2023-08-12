namespace Mana

type ParseErrorKind =
    | UnexpectedEof
    | UnexpectedChar of char
    | ExpectedDigit
    | ParseNum of string
    | BadIndentation
    | ExpectedToken of expected: Token * got: Token
    | ExpectedDefinition of got: Token

type ParseError = {
    kind: ParseErrorKind
    span: Span
}

type ParseResult<'t> = Result<'t, ParseError>
