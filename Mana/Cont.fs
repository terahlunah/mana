module Mana.Cont

type Partial<'T, 'R> = ('T -> 'R) -> 'R

type ContinuationBuilder() =
    member this.Return(x) = (fun k -> k x)
    member this.ReturnFrom(x) = x
    member this.Bind(m, f) = (fun k -> m (fun a -> f a k))
    member this.Delay(f) = f ()

let cps = ContinuationBuilder()

let run cont = cont id
