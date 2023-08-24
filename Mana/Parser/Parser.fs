namespace Mana.Parser

open System.Collections.Generic
open FSharpPlus
open FsToolkit.ErrorHandling
open Mana

type Parser(tokens: TokenSpan list) =
    let tokens = tokens
    let mutable index = 0
    let mutable span = Span.zero

    member self.error kind = {
        span = span
        kind = kind
    }

    member self.trace() =
        match (List.tryItem index tokens) with
        | Some ts -> printfn $"-> %A{ts.token} [%A{ts.data}] @ %O{ts.span}"
        | None -> ()

    member self.current() =
        List.tryItem index tokens
        |> Option.okOr (self.error ParseErrorKind.UnexpectedEof)

    member self.advance() =
        index <- index + 1

        match self.current () with
        | Ok ts -> span <- ts.span
        | _ -> ()

    member self.next() = result {
        let! ts = self.current ()
        self.advance ()
        return ts
    }

    member self.expect(kind) = result {
        let! ts = self.next ()

        if ts.token = kind then
            return ts
        else
            return! Error(self.error (ParseErrorKind.ExpectedToken(expected = kind, got = ts.token)))
    }

    member self.skip(kind) = result {
        let! ts = self.next ()

        if ts.token = kind then
            return ()
        else
            return! Error(self.error (ParseErrorKind.ExpectedToken(expected = kind, got = ts.token)))
    }

    member self.skipAll(kind) = result {
        let! ts = self.current ()

        if ts.token = kind then
            self.advance ()
            return! self.skipAll (kind)
        else
            return ()
    }

    member self.skipData(kind, data) = result {
        let! ts = self.next ()

        if ts.token = kind && ts.data = Some(data) then
            return ()
        else
            return! Error(self.error (ParseErrorKind.ExpectedToken(expected = kind, got = ts.token)))
    }

    member self.is(kind) =
        self.current ()
        |> Result.map (fun ts -> ts.token = kind)
        |> Result.defaultValue false

    member self.parseSeqDelimitedBy(left, right, p) = result {
        do! self.skip left

        let mutable items = []

        while not (self.is right) do
            let! item = p ()
            items <- items @ [ item ]

        do! self.skip right

        return items
    }

    member self.parseSeqUntil(until, p) = result {
        let mutable items = []

        while not (self.is until) do
            let! item = p ()
            items <- items @ [ item ]

        do! self.skip until

        return items
    }

    member self.parseSeqUntilData(until, data, p) = result {
        let mutable items = []

        while not (self.is until) do
            let! item = p ()
            items <- items @ [ item ]

        do! self.skipData (until, data)

        return items
    }

    member self.parse() : ParseResult<Program> = result {
        self.trace ()

        let mutable definitions = []
        let mutable modules = []

        let rec loop () = result {
            do! self.skipAll Token.NewLine
            let! ts = self.current ()

            match ts.token with
            | Mod ->
                let! m = self.parseMod ()
                modules <- m :: modules
                return! loop ()
            | Def ->
                let! d = self.parseDef ()
                printfn $"parse def %A{d}"
                definitions <- d :: definitions
                return! loop ()
            | _ ->
                printfn $"parse token %A{ts}"
                return ()
        }

        do! loop ()

        let rootModule = {
            name = ""
            definitions = definitions
        }

        let program = { modules = rootModule :: modules }

        return program
    }

    member self.parseMod() : ParseResult<Module> = result {
        do! self.skip Token.Mod

        let! nameToken = self.expect Token.Term
        let name = nameToken |> TokenSpan.asStr |> Option.unwrap

        do! self.skipData (Token.Operator, TokenData.Str "=")

        let! definitions = self.parseSeqDelimitedBy (Token.Indent, Token.Dedent, self.parseDef)

        return {
            name = name
            definitions = definitions
        }
    }

    member self.parseDef() : ParseResult<Definition> = result {
        printfn "parsing def"
        do! self.skip Token.Def

        let! nameToken = self.expect Token.Term
        let name = nameToken |> TokenSpan.asStr |> Option.unwrap // TODO: remove unwrap
        let! args = self.parseSeqUntilData (Token.Operator, TokenData.Str "=", self.parseArgument)
        let! body = self.parseExpr ()

        return {
            name = name
            args = args
            body = body
        }
    }

    member self.parseArgument() : ParseResult<Argument> = result {
        let! ts = self.current ()

        match ts.token with
        | Token.Term ->
            let! nameToken = self.expect Token.Term

            return
                nameToken
                |> TokenSpan.asStr
                |> Option.unwrap
                |> Argument.Named
        | Token.LParen ->
            do! self.skip Token.LParen
            do! self.skip Token.RParen
            return Argument.Unit
        | _ -> return! Error(self.error ParseErrorKind.ExpectedArgument)

    }

    member self.parseExpr() : ParseResult<Expr> = self.parseExpr (0)

    member self.parseExpr(p) : ParseResult<Expr> = result {
        let! left = self.parseElement ()

        let rec loop left = result {
            let! op = self.tryParseBinaryOperator (p)
            //TODO return error if unknown operator

            match op with
            | Some op ->
                let q =
                    match op.associativity with
                    | Left -> op.precedence + 1
                    | Right -> op.precedence

                let! right = self.parseExpr q
                let e = Expr.BinaryOp(op, left, right)
                return! loop e
            | None -> return left
        }

        return! loop left
    }

    member self.parseElement() : ParseResult<Expr> = result {

        let! ts = self.current ()

        let expr =
            match ts.token with
            | Token.Bool
            | Token.Num
            | Token.Char
            | Token.Str -> self.parseLiteral ()
            | Token.LParen -> self.parseParenExpr ()
            | Token.LBracket -> failwith "todo"
            | Token.LBrace -> failwith "todo"
            | Token.Eq -> failwith "todo"
            | Token.If -> failwith "todo"
            | Token.Match -> failwith "todo"
            | Token.Term -> self.parseTerm ()
            | Token.Operator -> result {
                let! op = self.parseUnaryOperator ()
                let q = op.precedence
                let! e = self.parseExpr q
                return UnaryOp(op, e)
              }
            | _ -> Error(self.error (ParseErrorKind.ExpectedExpr(got = ts.token)))

        return! expr
    }

    member self.isExpr(t) =
        match t with
        | Token.Bool
        | Token.Num
        | Token.Char
        | Token.Str
        | Token.LParen
        | Token.LBracket
        | Token.LBrace
        | Token.If
        | Token.Match
        | Token.Term
        | Token.Operator -> true
        | _ -> false

    member self.parseTerm() : ParseResult<Expr> = result {
        let! ts = self.expect Token.Term

        let symbol = ts |> TokenSpan.asStr |> Option.unwrap

        let! ts = self.current ()

        if self.isExpr ts.token then
            let! arg = self.parseExpr ()
            return Call(symbol, [ arg ])
        else
            return Ident symbol
    }

    member self.tryParseBinaryOperator(p) : ParseResult<Option<BinaryOperator>> = result {
        let! ts = self.current ()

        return!
            match ts.token with
            | Token.Operator -> self.parseBinaryOperator (p)
            | _ -> Ok None

    }

    member self.parseBinaryOperator(p) : ParseResult<Option<BinaryOperator>> = result {
        let! ts = self.current ()

        let symbol = ts |> TokenSpan.asStr |> Option.unwrap

        let! op =
            BinaryOperator.find symbol
            |> Option.okOr (self.error (ParseErrorKind.UnknownOperator symbol))

        if op.precedence >= p then
            self.advance ()
            return Some op
        else
            return None

    }

    member self.parseUnaryOperator() : ParseResult<UnaryOperator> = result {
        let! ts = self.expect Token.Operator

        let symbol = ts |> TokenSpan.asStr |> Option.unwrap

        return!
            UnaryOperator.find symbol
            |> Option.okOr (self.error ParseErrorKind.ExpectedOperator)
    }

    member self.parseLiteral() : ParseResult<Expr> = result {
        let! ts = self.current ()

        let expr =
            match ts.token with
            | Token.Bool -> ts |> TokenSpan.asBool |> Option.unwrap |> Expr.Bool |> Ok
            | Token.Num -> ts |> TokenSpan.asNum |> Option.unwrap |> Expr.Num |> Ok
            | Token.Char -> ts |> TokenSpan.asChar |> Option.unwrap |> Expr.Char |> Ok
            | Token.Str -> ts |> TokenSpan.asStr |> Option.unwrap |> Expr.Str |> Ok
            | _ -> Error(self.error (ParseErrorKind.ExpectedLiteral(got = ts.token)))

        self.advance ()

        return! expr
    }

    member self.parseParenExpr() : ParseResult<Expr> = result {
        do! self.skip Token.LParen

        let! ts = self.current ()

        if ts.token = Token.RParen then
            do! self.skip Token.RParen
            return Expr.Unit
        else
            let! expr = self.parseExpr ()
            do! self.skip Token.RParen
            return expr
    }

// member self.parseBracket() : ParseResult<Expr> = result {
//     do! self.skip Token.LBracket
//
//     let mutable exprs = []
//
//     let! ts = self.current ()
//
//     // Empty List
//     if ts.token = Token.RBracket then
//         do! self.skip Token.RBracket
//         return List []
//
//     // Empty Table
//     if ts.token = Token.Colon then
//         do! self.skip Token.Colon
//         do! self.skip Token.RBracket
//         return Table []
//
//     // Elements
//     let rec loop () = result {
//         let! ts = self.current ()
//
//         match ts.token with
//         | RBracket -> return ()
//     }
//
//     do! loop ()
//
//     do! self.skip Token.RBracket
//
//     return List []
// }
