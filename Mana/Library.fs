﻿namespace Mana

open System.IO
open System.Reflection
open Mana.Error
open Mana.Interop

type Mana() as this =
    let mutable globalEnv: Env<Value> = Env.empty ()

    do this.loadPrelude ()

    member this.loadPrelude() =
        globalEnv <- Env.merge globalEnv Builtins.env

        let prelude = this.loadResource "prelude"
        this.run prelude |> ignore

    member private this.loadResource(name) =
        let assembly = Assembly.GetExecutingAssembly()
        let resourceName = $"Mana.{name}.mana"

        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new StreamReader(stream)
        let content: string = reader.ReadToEnd()
        content

    member this.getValue(name: string) : Value =
        globalEnv.get name |> Option.defaultValue Value.Nil

    member this.get<'T>(name: string) : 'T =
        this.getValue name |> this.fromValue<'T>

    member this.setValue(name: string, value: Value) = globalEnv.set (name, value)

    member this.set<'T>(name: string, value: 'T) =
        let v = this.toValue value
        this.setValue (name, v)

    member this.run(code: string) : Value =
        let tokens = code |> Lexer.lex

        let ast = tokens |> Parser.parseMany

        let runScript = Compiler.compileExpr ast

        runScript globalEnv

    member this.call(fname, args: Value seq) : Value =
        let env = globalEnv.localScope ()

        match env.get fname with
        | Some(Value.Closure(handler)) -> handler env (args |> Seq.toList)
        | Some _ -> raiseError <| ManaError.NotAFunction fname
        | _ -> raiseError <| ManaError.FunctionNotFound fname

    member this.toValue<'T>(native: obj) : Value = fromNative globalEnv native
    member this.fromValue<'T>(v: Value) : 'T = fromValue<'T> globalEnv v
