namespace Mana

type Lexer = {
    source: string
    startPos: int
    currentPos: int
    indents: int list
}

module Lexer =
    let create source = {
        source = source
        startPos = 0
        currentPos = 0
        indents = []
    }

    let span lexer = {
        source = lexer.source
        start = lexer.startPos
        size = lexer.currentPos - lexer.startPos
    }

    let error kind (lexer: Lexer) = {
        kind = kind
        span = span lexer
    }

    let indentLevel (lexer: Lexer) =
        match lexer.indents with
        | [] -> 0
        | head :: _ -> head

    let hasIndent (level: int) (lexer: Lexer) = lexer.indents |> List.contains level

    let pushIndent (level: int) (lexer: Lexer) = { lexer with indents = level :: lexer.indents }

    let popIndent (level: int) (lexer: Lexer) =
        let mutable count = 0
        let mutable newIndents = lexer.indents

        while indentLevel lexer <> level do
            newIndents <- List.tail newIndents
            count <- count + 1

        { lexer with indents = newIndents }, count

    let current (lexer: Lexer) : char option = String.at lexer.currentPos lexer.source

    let peek (lexer: Lexer) : char option =
        String.at (lexer.currentPos + 1) lexer.source

    let advance (lexer: Lexer) : Lexer = { lexer with currentPos = lexer.currentPos + 1 }

    let isLineStart (lexer: Lexer) : bool =
        if lexer.currentPos > 1 then
            match String.at (lexer.currentPos - 1) lexer.source with
            | Some c -> c = '\n'
            | None -> false
        else
            true

    let trace (lexer: Lexer) =
        let src =
            lexer.source
            |> String.toList
            |> List.map (fun c ->
                match c with
                | '\n' -> "↵"
                | c -> string c
            )
            |> String.concat ""

        printfn "["
        printfn $"%A{lexer.indents}"
        printfn "]"
        printfn $"%s{src}"
        printfn "%s↑" (String.replicate lexer.currentPos " ")

    let isTermLead (c: char) : bool =
        match c with
        | _ when
            ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z')
            || c = '='
            || c = '+'
            || c = '-'
            || c = '*'
            || c = '/'
            || c = '>'
            || c = '<'
            || c = '_'
            || c = '|'
            || c = '^'
            || c = '%'
            || c = '?'
            || c = '!'
            || c = ':'
            || c = '~'
            ->
            true
        | _ -> false

    let isTerm (c: char) : bool =
        match c with
        | _ when
            ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z')
            || ('0' <= c && c <= '9')
            || c = '='
            || c = '+'
            || c = '-'
            || c = '*'
            || c = '/'
            || c = '>'
            || c = '<'
            || c = '_'
            || c = '|'
            || c = '^'
            || c = '%'
            || c = '?'
            || c = '!'
            || c = ':'
            || c = '~'
            ->
            true
        | _ -> false

    let read (lexer: Lexer) : Result<Lexer * char, ParseError> =
        match current lexer with
        | None -> Error(error ParseErrorKind.UnexpectedEof lexer)
        | Some c ->
            let updatedLexer = advance lexer
            Ok(updatedLexer, c)

    let tryReadFn (f: char -> bool) (lexer: Lexer) : Option<Lexer * char> =
        match current lexer with
        | Some c when f c ->
            let updatedLexer = advance lexer
            Some(updatedLexer, c)
        | _ -> None

    let tryReadExact (c: char) (lexer: Lexer) : Option<Lexer * unit> =
        match current lexer with
        | Some cur when cur = c ->
            let updatedLexer = advance lexer
            Some(updatedLexer, ())
        | _ -> None

    let readExact (c: char) (lexer: Lexer) : Result<Lexer, ParseError> =
        match tryReadExact c lexer with
        | Some(updatedLexer, _) -> Ok updatedLexer
        | None -> Error(error (ParseErrorKind.UnexpectedChar c) lexer)

    let tryReadDigit (lexer: Lexer) : Option<Lexer * char> =
        match current lexer with
        | Some c when '0' <= c && c <= '9' ->
            let updatedLexer = advance lexer
            Some(updatedLexer, c)
        | _ -> None

    let readDigit (lexer: Lexer) : Result<Lexer * char, ParseError> =
        match tryReadDigit lexer with
        | Some(updatedLexer, c) -> Ok(updatedLexer, c)
        | None -> Error(error ParseErrorKind.ExpectedDigit lexer)
