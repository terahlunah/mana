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
    | ExpectedTerm of got: Token
    | ExpectedOperator
    | ExpectedArgument
    | UnknownOperator of string

type ParseError = {
    kind: ParseErrorKind
    span: Span
} with

    member self.pretty() =
        $"%A{self.kind}\n%s{self.span.pretty ()}"

type ParseResult<'T> = Result<'T, ParseError>
