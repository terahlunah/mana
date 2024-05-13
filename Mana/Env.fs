namespace Mana

open System.Collections.Generic

type Env<'T> = {
    name: string
    bindings: Dictionary<string, 'T>
    mutable parent: Env<'T> option
    // This can be used to implement
    // - try catch : raise
    // - seq : yield
    // - proc : early return
    // - while/for : break
    effectHandlers: Stack<unit>

} with

    static member empty<'T>(name) : Env<'T> = {
        name = name
        bindings = Dictionary<string, 'T>()
        parent = None
        effectHandlers = Stack<unit>() 
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
        let rec loop e =
            match e.bindings.TryGetValue(k) with
            | true, v -> Some v
            | false, _ -> 
                match e.parent with
                | Some p -> loop p
                | _ -> None

        loop self

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

    let set k v (env: Env<_>) =
        env.set (k, v)
        env
