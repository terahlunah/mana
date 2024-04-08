namespace Mana

open Mana.Error
open Yute

type Mana() =
    let mutable globalEnv: Env<Value> = Env.empty ()

    member self.loadCore() =
        globalEnv <- Env.merge globalEnv Core.env

    member self.get(name: string) : Value =
        globalEnv.get name |> Option.defaultValue Value.Nil

    member self.set(name: string, value: Value) = globalEnv.set (name, value)

    member self.run(code: string) : Value =
        let tokens = code |> Lexer.lex

        for t in tokens do
            printfn $"%A{t.kind} %A{t.data}"

        let script = tokens |> Parser.parseMany

        let runScript = Compiler.compileScript script

        runScript globalEnv

    member self.call(fname, args) : Value =
        let env = globalEnv.localScope ()

        match env.get fname with
        | Some(Value.Closure(handler)) -> handler env args
        | Some _ -> raiseError <| ManaError.NotAFunction fname
        | _ -> raiseError <| ManaError.FunctionNotFound fname
