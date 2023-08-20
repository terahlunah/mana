namespace Mana.Parser

open System.Collections.Generic
open FSharpPlus
open FsToolkit.ErrorHandling
open Mana

type Parser(tokens: TokenSpan list) =
    let tokens = tokens
    let mutable index = 0
    let mutable span = Span.zero

    let mutable operators =
        let d = Dictionary<string, Operator>()

        d["+"] <- {
            name = "+"
            arity = 2
            precedence = 30
        }

        d

    member self.error kind = {
        span = span
        kind = kind
    }

    member self.trace() =
        let t =
            tokens
            |> List.map (fun ts -> $"%A{ts.token} %A{ts.data}\n")
            |> String.concat ""

        printf $"%s{t}"

        match (List.tryItem index tokens) with
        | Some ts ->
            printfn $"-> %A{ts.token}"
            ts.span |> Span.display
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

    member self.parse() : ParseResult<Program> = result {
        self.trace ()

        let! ts = self.current ()

        let mutable definitions = []
        let mutable modules = []

        match ts.token with
        | Mod ->
            let! m = self.parseMod ()
            modules <- m :: modules
        | Def ->
            let! d = self.parseDef ()
            definitions <- d :: definitions
        | _ -> return! Error(self.error (ParseErrorKind.ExpectedDefinition(got = ts.token)))

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

        do! self.skip Token.Eq

        let! definitions = self.parseSeqDelimitedBy (Token.Indent, Token.Dedent, self.parseDef)

        return {
            name = name
            definitions = definitions
        }
    }

    member self.parseDef() : ParseResult<Definition> = result {
        do! self.skip Token.Def

        let! nameToken = self.expect Token.Term
        let name = nameToken |> TokenSpan.asStr |> Option.unwrap // TODO: remove unwrap
        let! args = self.parseSeqUntil (Token.Eq, self.parseArgument)
        let! body = self.parseExpr ()

        return {
            name = name
            args = args
            body = body
        }
    }

    member self.parseArgument() : ParseResult<Argument> = result {
        let! nameToken = self.expect Token.Term
        let name = nameToken |> TokenSpan.asStr |> Option.unwrap // TODO: remove unwrap

        return Argument name
    }

    member self.parseExpr() : ParseResult<Expr> = self.parseExpr (0)

    member self.parseExpr(precedence) : ParseResult<Expr> = result {

        let! left = self.parseElement ()

        let! current = self.current ()

        let expr =
            match ts.token with
            | Token.Bool
            | Token.Num
            | Token.Char
            | Token.Str -> self.parseLiteral ()
            | Token.LParen -> self.parseParen ()
            | Token.LBracket -> self.parseBracket ()
            | Token.LBrace -> failwith "todo"
            | Token.Eq -> failwith "todo"
            | Token.If -> failwith "todo"
            | Token.Match -> failwith "todo"
            | Token.Term -> failwith "todo"
            | _ -> Error(self.error (ParseErrorKind.ExpectedExpr(got = ts.token)))

        return! expr
    }

    member self.parseElement() : ParseResult<Expr> = result {

        let! ts = self.current ()

        let expr =
            match ts.token with
            | Token.Bool
            | Token.Num
            | Token.Char
            | Token.Str -> self.parseLiteral ()
            | Token.LParen -> self.parseParen ()
            | Token.LBracket -> self.parseBracket ()
            | Token.LBrace -> failwith "todo"
            | Token.Eq -> failwith "todo"
            | Token.If -> failwith "todo"
            | Token.Match -> failwith "todo"
            | Token.Term -> failwith "todo"
            | _ -> Error(self.error (ParseErrorKind.ExpectedExpr(got = ts.token)))

        return! expr
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
