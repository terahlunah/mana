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

    let localScope env = { empty with parent = Some env }

    let withParent parent env = { env with parent = Some parent }

    let set k v env = { env with bindings = env.bindings |> Map.add k v }

    let rec get k env =
        env.bindings
        |> Map.tryFind k
        |> Option.orElse (env.parent |> Option.bind (get k))

    let rec flatten env =
        match env.parent with
        | Some p -> {
            bindings = Map.merge (flatten p).bindings env.bindings
            parent = None
          }
        | None -> env

    let merge e1 e2 = {
        bindings = Map.merge e1.bindings e2.bindings
        parent = None
    }
