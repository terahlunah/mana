﻿namespace Mana

open Mana
open Mana.Error
open Yute
open FsToolkit.ErrorHandling

type Parser(tokens: Token list) =
    let tokens = tokens
    let mutable index = 0
    let mutable span = Span.zero

    // do
    //     for t in tokens do
    //         displayEscaped t

    member this.error kind = kind

    member this.trace() =
        match (List.tryItem index tokens) with
        | Some ts -> printfn $"-> %A{ts.kind} [%A{ts.data}] @ %O{ts.span}"
        | None -> ()

    member this.current() =
        List.tryItem index tokens
        |> Option.orRaise (this.error ManaError.UnexpectedEof |> ManaException)

    member this.peek() = List.tryItem (index + 1) tokens

    member this.peekIs(kind) =
        this.peek ()
        |> Option.map (fun ts -> ts.kind = kind)
        |> Option.defaultValue false

    member this.advance() =
        if index < tokens.Length then
            index <- index + 1

            if index < tokens.Length then
                let ts = this.current ()
                span <- ts.span

    member this.advanceSkipNewLine() =
        this.advance ()

        while this.is TokenKind.NewLine do
            this.advance ()

    member this.next() =
        let ts = this.current ()
        this.advance ()
        ts

    member this.expect(kind) =
        let ts = this.next ()

        if ts.kind = kind then
            ts
        else
            raiseError (this.error (ManaError.ExpectedToken(expected = kind, got = ts.kind)))

    member this.skip(kind) =
        let ts = this.next ()

        if ts.kind <> kind then
            raiseError (this.error (ManaError.ExpectedToken(expected = kind, got = ts.kind)))

    member this.skipAll(kind) =
        let ts = this.current ()

        if ts.kind = kind then
            this.advance ()
            this.skipAll kind

    member this.trySkip(kind) =
        let ts = this.current ()

        if ts.kind = kind then
            this.advance ()
            true
        else
            false

    member this.skipData(kind, data) =
        let ts = this.next ()

        if ts.kind <> kind || ts.data <> Some(data) then
            raiseError (this.error (ManaError.ExpectedToken(expected = kind, got = ts.kind)))

    member this.is(kind) =
        let ts = this.current ()
        ts.kind = kind

    member this.isCloser() =
        let ts = this.current ()

        match ts.kind with
        | RBrace
        | RBracket
        | RParen
        | NewLine
        | Comma
        | Operator
        | Pipe
        | Eof -> true
        | _ -> false

    member this.parseDelimitedBy(left, right, p) =
        this.skip left
        let item = p ()
        this.skip right

        item

    member this.parseSeqDelimitedBy(left, right, p) =
        this.skip left

        let mutable items = []

        while not (this.is right) do
            let item = p ()
            items <- items @ [ item ]

        this.skip right

        items

    member this.parseSeqSeparatedBy(sep, p) =
        let mutable items = []

        let rec loop () =
            let e = p ()
            items <- items @ [ e ]

            if this.trySkip sep then
                loop ()

        loop ()

        items

    member this.parseSeqSeparatedAndDelimitedBy(left, sep, right, p) =
        this.skip left

        let mutable items = []

        let rec loop () =
            let ts = this.current ()

            match ts.kind with
            | t when t = right -> ()
            | _ ->
                let e = p ()
                items <- items @ [ e ]

                if this.trySkip sep then
                    loop ()

        loop ()

        this.skip right

        items

    member this.parseSeqUntil(until, p) =
        let mutable items = []

        while not (this.is until) do
            let item = p ()
            items <- items @ [ item ]

        this.skip until

        items

    member this.parseSeqUntilData(until, data, p) =
        let mutable items = []

        while not (this.is until) do
            let item = p ()
            items <- items @ [ item ]

        this.skipData (until, data)

        items

    member this.parseSeqSeparatedByUntilCond(cond, sep, p) =
        let mutable items = []

        let rec loop () =
            if cond () then
                ()
            else
                let e = p ()
                items <- items @ [ e ]

                if this.trySkip sep then
                    loop ()

        loop ()

        items

    member this.parseMany() : Ast =
        let mutable items = []

        this.skipAll TokenKind.NewLine

        while not (this.is TokenKind.Eof) do
            let e = this.parseExpr 0
            items <- items @ [ e ]
            this.skipAll TokenKind.NewLine

        items |> Ast.Block

    member this.parseGroup() : Ast =
        this.skip TokenKind.LParen

        this.skipAll TokenKind.NewLine

        this.parseSeqUntil (
            TokenKind.RParen,
            (fun _ ->
                this.skipAll TokenKind.NewLine
                let e = this.parseExpr ()
                this.skipAll TokenKind.NewLine
                e
            )
        )
        |> Ast.Block

    member this.parseList(p: unit -> 'T) : 'T list =
        this.parseSeqSeparatedAndDelimitedBy (
            TokenKind.LBracket,
            TokenKind.Comma,
            TokenKind.RBracket,
            (fun _ ->
                this.skipAll TokenKind.NewLine
                let e = p ()
                this.skipAll TokenKind.NewLine
                e
            )
        )

    member this.parseListExpr() : Ast =
        this.parseList this.parseExpr |> Ast.List

    member this.parseListPattern() : Pattern =

        let parseListItem () =
            if this.is TokenKind.Rest then
                this.skip TokenKind.Rest
                let p = this.expect TokenKind.Symbol

                p
                |> Token.asStr
                |> Option.unwrap
                |> CollectionPatternItem.Rest
            else
                this.parsePattern () |> CollectionPatternItem.Single

        this.parseList parseListItem |> Pattern.List

    member this.parseTable(p: unit -> 'T) : ('T * 'T) list =

        this.skip TokenKind.Hash

        this.parseSeqSeparatedAndDelimitedBy (
            TokenKind.LBracket,
            TokenKind.Comma,
            TokenKind.RBracket,
            (fun _ ->
                this.skipAll TokenKind.NewLine
                let k = p ()
                this.skip TokenKind.Colon
                let v = p ()
                this.skipAll TokenKind.NewLine
                k, v
            )
        )

    member this.parseTableExpr() : Ast =
        this.parseTable this.parseExpr |> Ast.Table

    member this.parseTablePattern() : Pattern = todo "Table pattern"
    // this.parseTable this.parsePattern |> Pattern.Table

    member this.parseTable() : Ast =

        this.skip TokenKind.Hash

        this.parseSeqSeparatedAndDelimitedBy (
            TokenKind.LBracket,
            TokenKind.Comma,
            TokenKind.RBracket,
            (fun _ ->
                this.skipAll TokenKind.NewLine
                let k = this.parseExpr 0
                this.skip TokenKind.Colon
                let v = this.parseExpr 0
                this.skipAll TokenKind.NewLine
                k, v
            )
        )
        |> Ast.Table

    member this.parseLet() : Ast =
        this.skip TokenKind.Let

        let pattern = this.parsePattern ()

        this.skip TokenKind.Eq

        let value = this.parseExpr ()

        let body = this.parseMany ()

        Ast.Let(pattern, value, body)

    member this.parseMatch() : Ast =
        this.skip TokenKind.Match

        let v = this.parseExpr ()

        this.skipAll TokenKind.NewLine

        this.skip TokenKind.Pipe

        let cases =
            this.parseSeqSeparatedBy (
                TokenKind.Pipe,
                (fun _ ->
                    this.skipAll TokenKind.NewLine
                    let pattern = this.parsePattern ()
                    this.skip TokenKind.Arrow
                    let body = this.parseExpr ()
                    this.skipAll TokenKind.NewLine

                    {
                        pattern = pattern
                        body = body
                    }
                )
            )

        Ast.Match(v, cases)

    member this.parseCall() : Ast =

        let name = this.current () |> Token.asStr |> Option.unwrap

        this.advance ()

        let mutable args = []

        if this.is TokenKind.LParen then
            args <-
                this.parseSeqSeparatedAndDelimitedBy (
                    TokenKind.LParen,
                    TokenKind.Comma,
                    TokenKind.RParen,
                    (fun _ ->
                        this.skipAll TokenKind.NewLine
                        let e = this.parseExpr 0
                        this.skipAll TokenKind.NewLine
                        e
                    )
                )

        while not <| this.isCloser () do
            // Trailing args
            args <- args @ [ this.parseExpr 100 ]

        Ast.Call(name, args)

    member this.parseArg() : string =
        this.expect TokenKind.Symbol |> Token.asStr |> Option.unwrap

    member this.parseLambda() : Ast =

        this.skip TokenKind.LBrace
        this.skipAll TokenKind.NewLine

        let mutable args =
            if this.is TokenKind.Pipe then
                this.parseSeqSeparatedAndDelimitedBy (
                    TokenKind.Pipe,
                    TokenKind.Comma,
                    TokenKind.Pipe,

                    (fun _ ->
                        this.skipAll TokenKind.NewLine
                        let a = this.parseArg ()
                        this.skipAll TokenKind.NewLine
                        a
                    )
                )
            else
                []

        this.skipAll TokenKind.NewLine

        let body =
            this.parseSeqUntil (
                TokenKind.RBrace,
                (fun _ ->
                    this.skipAll TokenKind.NewLine
                    let e = this.parseExpr ()
                    this.skipAll TokenKind.NewLine
                    e
                )
            )

        // Add Implicit "it" argument when needed
        if args.Length = 0 && Ast.useImplicitIt body then
            args <- [ "it" ]

        Ast.Closure(args, body |> Ast.Block)

    member this.parseElement() : Ast =
        let ts = this.current ()

        match ts.kind with
        | TokenKind.LParen -> this.parseGroup ()
        | TokenKind.LBracket -> this.parseListExpr ()
        | TokenKind.Hash -> this.parseTableExpr ()
        | TokenKind.LBrace -> this.parseLambda ()
        | TokenKind.Let -> this.parseLet ()
        | TokenKind.Match -> this.parseMatch ()
        | TokenKind.Nil ->
            this.advance ()
            Ast.Nil
        | TokenKind.Bool ->
            this.advance ()
            Ast.Bool(Token.asBool ts |> Option.unwrap)
        | TokenKind.Num ->
            this.advance ()
            Ast.Num(Token.asNum ts |> Option.unwrap)
        | TokenKind.Str ->
            this.advance ()
            Ast.Str(Token.asStr ts |> Option.unwrap)
        | TokenKind.Symbol -> this.parseCall ()
        | TokenKind.Operator ->
            let op = this.parseUnaryOperator ()
            let q = op.precedence
            let e = this.parseExpr q
            Ast.Call(op.handler, [ e ])
        | _ -> raiseError (this.error (ExpectedExpr ts.kind))

    member this.tryParseBinaryOperator(p) : Option<BinaryOperator> =
        if this.is TokenKind.Operator then
            this.parseBinaryOperator p
        else
            None

    member this.parseBinaryOperator(p) : Option<BinaryOperator> =
        let ts = this.current ()

        let symbol = ts |> Token.asStr |> Option.unwrap

        let op =
            BinaryOperator.find symbol
            |> Option.orRaise (this.error (ManaError.UnknownOperator symbol |> ManaException))

        if op.precedence >= p then
            this.advance ()
            Some op
        else
            None

    member this.parseUnaryOperator() : UnaryOperator =
        let ts = this.expect TokenKind.Operator

        let symbol = ts |> Token.asStr |> Option.unwrap

        UnaryOperator.find symbol
        |> Option.orRaise (this.error ManaError.ExpectedOperator |> ManaException)

    member this.isUnaryOperator(ts) : bool =
        if not <| this.is TokenKind.Operator then
            false
        else
            let symbol = ts |> Token.asStr |> Option.unwrap
            UnaryOperator.find symbol |> Option.isSome

    member this.parsePattern() : Pattern =
        let ts = this.current ()

        match ts.kind with
        | TokenKind.Nil ->
            this.skip TokenKind.Nil
            Pattern.Nil
        | TokenKind.Wildcard ->
            this.skip TokenKind.Wildcard
            Pattern.Wildcard
        | TokenKind.Bool ->
            let p = this.expect TokenKind.Bool
            p |> Token.asBool |> Option.unwrap |> Pattern.Bool
        | TokenKind.Num ->
            let p = this.expect TokenKind.Num
            p |> Token.asNum |> Option.unwrap |> Pattern.Num
        | TokenKind.Str ->
            let p = this.expect TokenKind.Str
            p |> Token.asStr |> Option.unwrap |> Pattern.Str
        | TokenKind.Symbol ->
            let p = this.expect TokenKind.Symbol
            p |> Token.asStr |> Option.unwrap |> Pattern.Symbol
        | TokenKind.LBracket -> this.parseListPattern ()
        | TokenKind.Hash -> this.parseTablePattern ()
        | _ -> raiseError (this.error (ManaError.ExpectedExpr(got = ts.kind)))

    member this.parseExpr() : Ast = this.parseExpr 0

    member this.parseExpr(p: int) : Ast =
        let left = this.parseElement ()

        let rec loop left =
            let op = this.tryParseBinaryOperator p

            match op with
            | Some op ->
                let q =
                    match op.associativity with
                    | Left -> op.precedence + 1
                    | Right -> op.precedence

                let right = this.parseExpr q
                let e = Ast.Call(op.handler, [ left; right ])
                loop e
            | None -> left

        loop left

module Parser =

    let parseExprRaw tokens = Parser(tokens).parseExpr ()
    let parseManyRaw tokens = Parser(tokens).parseMany ()

    let parseExpr = parseExprRaw >> Ast.optimizeAndDesugar
    let parseMany = parseManyRaw >> Ast.optimizeAndDesugar
