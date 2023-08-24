namespace Mana.Parser

open Mana
open FsToolkit.ErrorHandling

type Lexer(source) =
    let mutable source: string = source
    let mutable startPos: int = 0
    let mutable currentPos: int = 0
    let mutable indents: int list = []
    let mutable tokens: TokenSpan list = []

    let isTermLead (c: char) : bool =
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

    let isOperator (c: char) : bool =
        c = '='
        || c = '+'
        || c = '-'
        || c = '*'
        || c = '/'
        || c = '>'
        || c = '<'
        || c = '|'
        || c = '^'
        || c = '%'
        || c = '?'
        || c = '!'
        || c = ':'
        || c = '~'

    let isTerm (c: char) : bool = ('0' <= c && c <= '9') || isTermLead c

    member self.span() = {
        source = source
        start = startPos
        size = currentPos - startPos
    }

    member self.error kind = {
        kind = kind
        span = self.span ()
    }

    member self.indentLevel() =
        match indents with
        | [] -> 0
        | head :: _ -> head

    member self.hasIndent(level: int) = indents |> List.contains level

    member self.pushIndent(level: int) = indents <- level :: indents

    member self.popIndent(level: int) =
        let mutable count = 0

        while self.indentLevel () <> level do
            indents <- List.tail indents
            count <- count + 1

        count

    member self.current() : char option = String.at currentPos source

    member self.peek() : char option = String.at (currentPos + 1) source

    member self.advance() = currentPos <- currentPos + 1

    member self.emit(token) = tokens <- tokens @ [ token ]

    member self.token(token) =
        token
        |> TokenSpan.ofToken
        |> TokenSpan.withSpan (self.span ())

    member self.token(token, b: bool) =
        self.token token |> TokenSpan.withBool b

    member self.token(token, n: double) = self.token token |> TokenSpan.withNum n

    member self.token(token, c: char) =
        self.token token |> TokenSpan.withChar c

    member self.token(token, s: string) = self.token token |> TokenSpan.withStr s

    member self.isLineStart() : bool =
        if currentPos > 1 then
            match String.at (currentPos - 1) source with
            | Some c -> c = '\n'
            | None -> false
        else
            true

    member self.trace() =
        let src =
            source
            |> String.toList
            |> List.map (fun c ->
                match c with
                | '\r' -> ""
                | '\n' -> "\\n"
                | c -> string c
            )
            |> String.concat ""

        printf "["

        for i in indents do
            printf $"%A{i},"

        printfn "]"
        printfn $"%s{src}"
        printfn "%s↑" (String.replicate currentPos " ")

    member self.lex() = result {

        let rec loop () = result {
            let! hasMore = self.readToken ()
            if hasMore then return! loop () else return ()
        }

        do! loop ()

        self.token Token.Eof |> self.emit

        return tokens
    }

    member self.readToken() : ParseResult<bool> = result {
        if self.isLineStart () then
            do! self.readIndent ()

            match self.current () with
            | Some ' ' -> do! (self.readToken () |> Result.ignore)
            | _ -> ()

        let! some = result {
            match self.current () with
            | Some c ->
                match c with
                | c when c >= '0' && c <= '9' -> do! self.readNum ()
                | '\'' -> do! self.readChar ()
                | '"' -> do! self.readStr ()
                | '{' ->
                    self.advance ()
                    self.token Token.LBrace |> self.emit
                | '}' ->
                    self.advance ()
                    self.token Token.RBrace |> self.emit
                | '(' ->
                    self.advance ()
                    self.token Token.LParen |> self.emit
                | ')' ->
                    self.advance ()
                    self.token Token.RParen |> self.emit
                | '[' ->
                    self.advance ()
                    self.token Token.LBracket |> self.emit
                | ']' ->
                    self.advance ()
                    self.token Token.RBracket |> self.emit
                | '#' -> do! self.skipComment ()
                | c when isOperator c -> self.readOperator ()
                | c when isTermLead c -> self.readTerm ()
                | c -> return! Error(self.error (ParseErrorKind.UnexpectedChar c))

                do! self.skipWhitespace ()
                return true
            | None -> return false
        }

        return some
    }

    member self.skipComment() : Result<unit, ParseError> = result {
        do! self.readExact ('#')

        let rec loop () =
            match self.current () with
            | Some '\n'
            | None -> ()
            | Some _ ->
                self.advance ()
                loop ()

        return loop ()
    }

    member self.skipWhitespace() = result {
        let rec loop () =
            match self.current () with
            | Some ' '
            | Some '\r' ->
                self.advance ()
                loop ()
            | Some '\n' ->
                self.token Token.NewLine |> self.emit
                self.advance ()
            | _ -> ()

        loop ()
        startPos <- currentPos
        return ()
    }

    member self.read() : Result<char, ParseError> =
        match self.current () with
        | None -> Error(self.error ParseErrorKind.UnexpectedEof)
        | Some c ->
            self.advance ()
            Ok(c)

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

    member self.readExact(c: char) : ParseResult<unit> =
        c
        |> self.tryReadExact
        |> thenOkOr () (self.error (ParseErrorKind.UnexpectedChar c))

    member self.tryReadDigit() : Option<char> =
        match self.current () with
        | Some c when '0' <= c && c <= '9' ->
            self.advance ()
            Some c
        | _ -> None

    member self.readDigit() : ParseResult<char> =
        self.tryReadDigit ()
        |> Option.okOr (self.error ParseErrorKind.ExpectedDigit)

    member self.readInt() : ParseResult<string> = result {
        let mutable num = ""
        let! digit = self.readDigit ()
        num <- num + string digit

        let rec loop () =
            match self.tryReadDigit () with
            | Some d ->
                num <- num + string d
                loop ()
            | _ -> ()

        loop ()
        return num
    }

    member self.readNum() : ParseResult<unit> = result {
        let! num = self.readInt ()

        let mutable num = num

        if self.tryReadExact '.' then
            let! fract = self.readInt ()
            num <- $"%s{num}.%s{fract}"

        let! num =
            parseFloat num
            |> Option.okOr (self.error (ParseErrorKind.ParseNum num))

        self.token Token.Num |> TokenSpan.withNum num |> self.emit
    }

    member self.readChar() : ParseResult<unit> = result {
        do! self.readExact '\''
        let! c = self.read ()
        do! self.readExact '\''
        self.token Token.Char |> TokenSpan.withChar c |> self.emit
    }

    member self.readStr() : ParseResult<unit> = result {
        let mutable s = ""

        do! self.readExact '"'

        let rec loop () = result {
            let! c = self.read ()

            if c = '"' then
                return ()
            else
                s <- s + string c
                return! loop ()
        }

        do! loop ()

        self.token Token.Str |> TokenSpan.withStr s |> self.emit
    }

    member self.readOperator() =
        let mutable op = ""

        whileSome (fun _ -> self.tryReadFn isOperator) (fun c -> op <- op + string c)

        self.token Token.Operator
        |> TokenSpan.withStr op
        |> self.emit

    member self.readTerm() =
        let mutable id = ""

        whileSome (fun _ -> self.tryReadFn isTerm) (fun c -> id <- id + string c)

        match id with
        | "def" -> self.token Token.Def
        | "=" -> self.token Token.Eq
        | "if" -> self.token Token.If
        | "then" -> self.token Token.Then
        | "else" -> self.token Token.Else
        | "match" -> self.token Token.Match
        | "with" -> self.token Token.With
        | "->" -> self.token Token.Arrow
        | "," -> self.token Token.Comma
        | ":" -> self.token Token.Colon
        | _ -> self.token Token.Term |> TokenSpan.withStr id
        |> self.emit

    member self.readIndent() : ParseResult<unit> = result {
        let mutable count = 0

        while self.tryReadExact ' ' do
            count <- count + 1

        match self.current () with
        | Some '\r'
        | Some '\n' -> do! self.skipWhitespace ()
        | _ ->
            if count > self.indentLevel () then
                self.pushIndent count
                self.token Token.Indent |> self.emit
            elif count < self.indentLevel () then
                if self.hasIndent count then
                    let count = self.popIndent count

                    for _ in 0..count do
                        self.token Token.Dedent |> self.emit
                else
                    return! Error(self.error ParseErrorKind.BadIndentation)
    }
