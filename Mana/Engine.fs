namespace Mana

open Mana
open Mana.Parser
open Mana.Compiler
open FsToolkit.ErrorHandling

type Engine = { env: Env<Value> }

module Engine =

    let init: Engine = { env = Env.empty }

    let set k v engine = { engine with env = Env.set k v engine.env }

    let loadProgram (program: Program) engine = result {
        let! env = compileProgram program engine.env
        return { engine with env = env }
    }

    let run fname args engine = result {
        let env = engine.env |> Env.localScope

        let! f =
            env
            |> Env.get fname
            |> Option.okOr (RuntimeError.FunctionNotFound fname)

        let! handler =
            match f with
            | Value.Fun(handler) -> Ok handler
            | _ -> Error RuntimeError.NotAFunction

        let! _, v = handler env args

        return v
    }

    let withStd engine =
        engine
        |> set "add" (Value.Fun Mana.Std.Core.add)
        |> set "neg" (Value.Fun Mana.Std.Core.neg)
        |> set "sub" (Value.Fun Mana.Std.Core.sub)
        |> set "mul" (Value.Fun Mana.Std.Core.mul)
        |> set "div" (Value.Fun Mana.Std.Core.div)
        |> set "print" (Value.Fun Mana.Std.Core.display)
