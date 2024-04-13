namespace Mana

open System.Collections.Generic

type Env<'T> = {
    mutable bindings: Dictionary<string, 'T>
    mutable parent: Env<'T> option
} with

    static member empty<'T>() : Env<'T> = {
        bindings = Dictionary<string, 'T>()
        parent = None
    }

    member self.localScope() = { Env<'T>.empty () with parent = Some self }
    member self.set(k, v) = self.bindings.[k] <- v

    member self.get k =
        let mutable env = self
        let mutable brk = false
        let mutable result = None

        while not brk do
            match env.bindings.TryGetValue(k) with
            | true, v ->
                result <- Some v
                brk <- true
            | false, _ ->
                match env.parent with
                | Some p -> env <- p
                | None -> brk <- true

        result

    member this.merge other =
        for KeyValue(k, v) in other.bindings do
            this.bindings.[k] <- v

module Env =
    let merge e1 e2 =
        let newBindings = Dictionary<string, 'T>(e1.bindings)

        for KeyValue(k, v) in e2.bindings do
            newBindings.[k] <- v

        {
            bindings = newBindings
            parent = None
        }

    let get k (env: Env<_>) = env.get k

    let set k v (env: Env<_>) =
        env.set (k, v)
        env
