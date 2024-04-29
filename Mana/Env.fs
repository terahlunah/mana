namespace Mana

open System.Collections.Generic

type Env<'T> = {
    name: string
    mutable bindings: Dictionary<string, 'T>
    mutable parent: Env<'T> option
} with

    static member empty<'T>(name) : Env<'T> = {
        name = name
        bindings = Dictionary<string, 'T>()
        parent = None
    }

    member self.localScope(name) = { Env<'T>.empty (name) with parent = Some self }
    member self.set(k, v) = self.bindings.[k] <- v
    
    member self.assign(k, v) =
        if self.bindings.ContainsKey k then
            self.bindings.[k] <- v
            true
        else
            false
            
    member self.globalAssign(k, v) =
        if self.bindings.ContainsKey k then
            self.bindings.[k] <- v
            true
        else
            self.parent |> Option.map (_.globalAssign(k,v)) |> Option.defaultValue false

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

    member this.merge(other) =
        for KeyValue(k, v) in other.bindings do
            this.bindings.[k] <- v
            
    member this.getAll():Dictionary<string, 'T> =
        
        match this.parent with
        | Some parent ->
            let newBindings = Dictionary<string, 'T>(parent.getAll())
            for KeyValue(k, v) in this.bindings do
                newBindings.[k] <- v
            newBindings
        | None -> Dictionary<string, 'T>(this.bindings)
        
    member this.scopes() =
        this.name :: (this.parent |> Option.map _.scopes() |> Option.defaultValue [])

module Env =
    let merge e1 e2 =
        let newBindings = Dictionary<string, 'T>(e1.bindings)

        for KeyValue(k, v) in e2.bindings do
            newBindings.[k] <- v

        {
            name = e1.name
            bindings = newBindings
            parent = None
        }

    let get k (env: Env<_>) = env.get k

    let set k v (env: Env<_>) =
        env.set (k, v)
        env
