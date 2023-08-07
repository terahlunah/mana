namespace Mana

type Env<'t> = { bindings: Map<string, 't> }

module Env =
    let empty = { bindings = Map.empty }

    let set k v env = { env with bindings = env.bindings |> Map.add k v }
    let get k env = env.bindings |> Map.tryFind k
