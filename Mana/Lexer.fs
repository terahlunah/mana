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

    member self.span() = {
        source = source
        start = startPos
        size = currentPos - startPos
    }

    member self.makeError kind = kind
    member self.current() : char option = String.at currentPos source

    member self.peek() : char option = String.at (currentPos + 1) source

    member self.advance() = currentPos <- currentPos + 1

    member self.emit(token) = tokens <- tokens @ [ token ]

    member self.token(token) =
        token |> Token.make |> Token.withSpan (self.span ())

    member self.token(token, b: bool) = self.token token |> Token.withBool b

    member self.token(token, n: double) = self.token token |> Token.withNum n

    member self.token(token, s: string) = self.token token |> Token.withStr s

    member self.trace() =
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

    member self.lex() =

        let rec loop () =
            let hasMore = self.readToken ()
            if hasMore then loop () else ()

        loop ()

        self.advance ()
        self.token TokenKind.Eof |> self.emit

        tokens

    member self.readToken() : bool =
        self.skipWhitespace ()

        match self.current () with
        | Some c ->
            match c with
            | c when c >= '0' && c <= '9' -> self.readNum ()
            | '"' -> self.readStr ()
            | '{' ->
                self.advance ()
                self.token TokenKind.LBrace |> self.emit
            | '}' ->
                self.advance ()
                self.token TokenKind.RBrace |> self.emit
            | '(' ->
                self.advance ()
                self.token TokenKind.LParen |> self.emit
            | ')' ->
                self.advance ()
                self.token TokenKind.RParen |> self.emit
            | '[' ->
                self.advance ()
                self.token TokenKind.LBracket |> self.emit
            | ']' ->
                self.advance ()
                self.token TokenKind.RBracket |> self.emit
            | '|' ->
                self.advance ()
                self.token TokenKind.Pipe |> self.emit
            | '#' ->
                self.advance ()
                self.token TokenKind.Hash |> self.emit
            | ',' ->
                self.advance ()
                self.token TokenKind.Comma |> self.emit
            | ';' -> self.readComment ()
            | c when isSymbolHead c -> self.readSymbol ()
            | c when isOperator c -> self.readOperator ()
            | c -> raiseError (self.makeError (ManaError.UnexpectedChar c))

            true
        | None -> false

    member self.readComment() =
        self.advance ()

        let mutable comment = ""

        let rec loop () =
            match self.current () with
            | Some '\n'
            | None -> ()
            | Some c ->
                if c <> '\r' then
                    comment <- comment + string c

                self.advance ()
                loop ()

        loop ()

    // self.token TokenKind.Comment
    // |> Token.withStr (comment.Trim())
    // |> self.emit

    member self.skipWhitespace() =
        startPos <- currentPos

        let rec loop () =
            match self.current () with
            | Some(' ' | '\r' | '\t') ->
                self.advance ()
                loop ()
            | Some('\n') ->
                self.advance ()
                self.token TokenKind.NewLine |> self.emit
                loop ()
            | _ -> ()

        loop ()
        startPos <- currentPos

    member self.read() : char =
        match self.current () with
        | None -> raiseError (self.makeError ManaError.UnexpectedEof)
        | Some c ->
            self.advance ()
            c

    member self.tryReadFn(f: char -> bool) : Option<char> =
        match self.current () with
        | Some c when f c ->
            self.advance ()
            Some c
        | _ -> None

    member self.tryReadExact(c: char) : bool =
        match self.current () with
        | Some cur when cur = c ->
            self.advance ()
            true
        | _ -> false

    member self.readExact(c: char) =
        if not (self.tryReadExact c) then
            raiseError (self.makeError (ManaError.ExpectedChar c))

    member self.tryReadDigit() : Option<char> =
        match self.current () with
        | Some c when '0' <= c && c <= '9' ->
            self.advance ()
            Some c
        | _ -> None

    member self.readDigit() : char =
        match self.tryReadDigit () with
        | Some d -> d
        | None -> raiseError (self.makeError ManaError.ExpectedDigit)

    member self.readInt() : string =
        let mutable num = ""
        let digit = self.readDigit ()
        num <- num + string digit

        let rec loop () =
            match self.tryReadDigit () with
            | Some d ->
                num <- num + string d
                loop ()
            | _ -> ()

        loop ()
        num

    member self.readNum() =
        let num = self.readInt ()

        let mutable num = num

        if self.tryReadExact '.' then
            let fract = self.readInt ()
            num <- $"%s{num}.%s{fract}"

        let num =
            Float.parse num
            |> Option.orRaise (ManaException(self.makeError (ManaError.ParseNum num)))

        self.token TokenKind.Num |> Token.withNum num |> self.emit

    member self.readStr() =
        let mutable s = ""

        do self.readExact '"'

        let rec loop () =
            let c = self.read ()

            if c = '"' then
                ()
            else
                s <- s + string c
                loop ()

        loop ()

        self.token TokenKind.Str |> Token.withStr s |> self.emit

    member self.readOperator() =
        let mutable op = ""

        whileSome (fun _ -> self.tryReadFn isOperator) (fun c -> op <- op + string c)

        match op with
        | "=" -> self.token TokenKind.Eq
        | "|" -> self.token TokenKind.Pipe
        | ":" -> self.token TokenKind.Colon
        | _ -> self.token TokenKind.Operator |> Token.withStr op
        |> self.emit

    member self.readSymbol() =
        let mutable id = ""

        whileSome (fun _ -> self.tryReadFn isSymbol) (fun c -> id <- id + string c)

        match id with
        | "nil" -> self.token TokenKind.Nil
        | "true" -> self.token TokenKind.Bool |> Token.withBool true
        | "false" -> self.token TokenKind.Bool |> Token.withBool false
        | "let" -> self.token TokenKind.Let
        | _ -> self.token TokenKind.Symbol |> Token.withStr id
        |> self.emit

module Lexer =
    let lex code = Lexer(code).lex ()
