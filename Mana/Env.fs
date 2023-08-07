namespace Mana

type Env<'t> = {
    bindings: Map<string, 't>
    parent: Env<'t> option
}

module Env =
    let empty = {
        bindings = Map.empty
        parent = None
    }

    let scoped env = { empty with parent = env }

    let set k v env = { env with bindings = env.bindings |> Map.add k v }

    let rec get k env =
        env.bindings
        |> Map.tryFind k
        |> Option.orElse (env.parent |> Option.bind (get k))
