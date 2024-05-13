namespace Mana

open System.IO
open System.Reflection
open Mana.Error
open Mana.Interop

type Mana() as this =
    let globalEnv: Env<Value> = Env.empty ("global")
    let ctx = Context<Value>()

    do this.loadPrelude ()

    member this.loadPrelude() =
        globalEnv.merge Builtins.env

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

    member this.parse(code: string) : Expr =
        let tokens = code |> Lexer.lex
        let ast = tokens |> Parser.parseMany
        ast

    member this.run(code: string) : Value =
        let tokens = code |> Lexer.lex
        let ast = tokens |> Parser.parseMany

        // let d = sprintf $"%A{ast}"
        // printfn "%s" (d.Replace("\n", "\r\n"))

        let runScript = Compiler.compile ast

        runScript ctx globalEnv

    member this.call(fname, args: Value seq) : Value =
        let env = globalEnv.localScope ("host call")

        match env.get fname with
        | Some(Value.Closure(handler)) -> handler ctx env (args |> Seq.toList) id
        | Some _ -> raiseError <| ManaError.NotAFunction fname
        | _ -> raiseError <| ManaError.FunctionNotFound fname

    member this.toValue<'T>(native: obj) : Value = fromNative ctx globalEnv native
    member this.fromValue<'T>(v: Value) : 'T = fromValue<'T> ctx globalEnv v
