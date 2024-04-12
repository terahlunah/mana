namespace Mana

open Mana
open Mana.Error
open Yute
open FsToolkit.ErrorHandling

type Lexer(source: string) =
    let mutable source: string = source
    let mutable startPos: int = 0
    let mutable currentPos: int = 0
    let mutable tokens: Token list = []

    let isSymbolHead (c: char) : bool =
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

    let isSymbol (c: char) : bool =
        ('0' <= c && c <= '9') || c = '?' || isSymbolHead c

    let isOperator (c: char) : bool =
        c = '='
        || c = '+'
        || c = '-'
        || c = '*'
        || c = '/'
        || c = '>'
        || c = '<'
        || c = '^'
        || c = '%'
        // || c = '?'
        || c = '!'
        || c = ':'
        || c = '~'
        || c = '.'

    member this.span() = {
        source = source
        start = startPos
        size = currentPos - startPos
    }

    member this.makeError kind = kind
    member this.current() : char option = String.at currentPos source

    member this.peek() : char option = String.at (currentPos + 1) source

    member this.advance() = currentPos <- currentPos + 1

    member this.emit(token) = tokens <- tokens @ [ token ]

    member this.token(token) =
        token |> Token.make |> Token.withSpan (this.span ())

    member this.token(token, b: bool) = this.token token |> Token.withBool b

    member this.token(token, n: double) = this.token token |> Token.withNum n

    member this.token(token, s: string) = this.token token |> Token.withStr s

    member this.trace() =
        let src =
            source
            |> String.toList
            |> List.map (fun c ->
                match c with
                | '\r' -> ""
                | '\n' -> "→"
                | c -> string c
            )
            |> String.concat ""

        printfn $"%s{src}"
        printfn "%s↑" (String.replicate currentPos " ")

    member this.lex() =

        let rec loop () =
            let hasMore = this.readToken ()
            if hasMore then loop () else ()

        loop ()

        this.advance ()
        this.token TokenKind.Eof |> this.emit

        tokens

    member this.readToken() : bool =
        this.skipWhitespace ()

        match this.current () with
        | Some c ->
            match c with
            | c when c >= '0' && c <= '9' -> this.readNum ()
            | '"' -> this.readStr ()
            | '{' ->
                this.advance ()
                this.token TokenKind.LBrace |> this.emit
            | '}' ->
                this.advance ()
                this.token TokenKind.RBrace |> this.emit
            | '(' ->
                this.advance ()
                this.token TokenKind.LParen |> this.emit
            | ')' ->
                this.advance ()
                this.token TokenKind.RParen |> this.emit
            | '[' ->
                this.advance ()
                this.token TokenKind.LBracket |> this.emit
            | ']' ->
                this.advance ()
                this.token TokenKind.RBracket |> this.emit
            | '|' ->
                this.advance ()
                this.token TokenKind.Pipe |> this.emit
            | '#' ->
                this.advance ()
                this.token TokenKind.Hash |> this.emit
            | ',' ->
                this.advance ()
                this.token TokenKind.Comma |> this.emit
            | ';' -> this.readComment ()
            | c when isSymbolHead c -> this.readSymbol ()
            | c when isOperator c -> this.readOperator ()
            | c -> raiseError (this.makeError (ManaError.UnexpectedChar c))

            true
        | None -> false

    member this.readComment() =
        this.advance ()

        let mutable comment = ""

        let rec loop () =
            match this.current () with
            | Some '\n'
            | None -> ()
            | Some c ->
                if c <> '\r' then
                    comment <- comment + string c

                this.advance ()
                loop ()

        loop ()

    // this.token TokenKind.Comment
    // |> Token.withStr (comment.Trim())
    // |> this.emit

    member this.skipWhitespace() =
        startPos <- currentPos

        let rec loop () =
            match this.current () with
            | Some(' ' | '\r' | '\t') ->
                this.advance ()
                loop ()
            | Some('\n') ->
                this.advance ()
                this.token TokenKind.NewLine |> this.emit
                loop ()
            | _ -> ()

        loop ()
        startPos <- currentPos

    member this.read() : char =
        match this.current () with
        | None -> raiseError (this.makeError ManaError.UnexpectedEof)
        | Some c ->
            this.advance ()
            c

    member this.tryReadFn(f: char -> bool) : Option<char> =
        match this.current () with
        | Some c when f c ->
            this.advance ()
            Some c
        | _ -> None

    member this.tryReadExact(c: char) : bool =
        match this.current () with
        | Some cur when cur = c ->
            this.advance ()
            true
        | _ -> false

    member this.readExact(c: char) =
        if not (this.tryReadExact c) then
            raiseError (this.makeError (ManaError.ExpectedChar c))

    member this.tryReadDigit() : Option<char> =
        match this.current () with
        | Some c when '0' <= c && c <= '9' ->
            this.advance ()
            Some c
        | _ -> None

    member this.readDigit() : char =
        match this.tryReadDigit () with
        | Some d -> d
        | None -> raiseError (this.makeError ManaError.ExpectedDigit)

    member this.readInt() : string =
        let mutable num = ""
        let digit = this.readDigit ()
        num <- num + string digit

        let rec loop () =
            match this.tryReadDigit () with
            | Some d ->
                num <- num + string d
                loop ()
            | _ -> ()

        loop ()
        num

    member this.readNum() =
        let num = this.readInt ()

        let mutable num = num

        if this.tryReadExact '.' then
            let fract = this.readInt ()
            num <- $"%s{num}.%s{fract}"

        let num =
            Float.parse num
            |> Option.orRaise (ManaException(this.makeError (ManaError.ParseNum num)))

        this.token TokenKind.Num |> Token.withNum num |> this.emit

    member this.readStr() =
        let mutable s = ""

        do this.readExact '"'

        let rec loop () =
            let c = this.read ()

            if c = '"' then
                ()
            else
                s <- s + string c
                loop ()

        loop ()

        this.token TokenKind.Str |> Token.withStr s |> this.emit

    member this.readOperator() =
        let mutable op = ""

        whileSome (fun _ -> this.tryReadFn isOperator) (fun c -> op <- op + string c)

        match op with
        | "=" -> this.token TokenKind.Eq
        | "|" -> this.token TokenKind.Pipe
        | ":" -> this.token TokenKind.Colon
        | "->" -> this.token TokenKind.Arrow
        | ".." -> this.token TokenKind.Rest
        | _ -> this.token TokenKind.Operator |> Token.withStr op
        |> this.emit

    member this.readSymbol() =
        let mutable id = ""

        whileSome (fun _ -> this.tryReadFn isSymbol) (fun c -> id <- id + string c)

        match id with
        | "nil" -> this.token TokenKind.Nil
        | "true" -> this.token TokenKind.Bool |> Token.withBool true
        | "false" -> this.token TokenKind.Bool |> Token.withBool false
        | "let" -> this.token TokenKind.Let
        | "match" -> this.token TokenKind.Match
        | "_" -> this.token TokenKind.Wildcard
        | _ -> this.token TokenKind.Symbol |> Token.withStr id
        |> this.emit

module Lexer =
    let lex code = Lexer(code).lex ()
