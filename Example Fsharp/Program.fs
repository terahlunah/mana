open System
open Mana

let m = Mana()

// Mana calling Native

let add1 env args =
    match args with
    | [ Value.Num a; Value.Num b ] -> a + b |> Value.Num
    | _ -> raise (ArgumentException "Arguments must be numbers")

let add2 a b : int = a + b

m.set ("add1", add1)
m.set ("add2", add2)

m.set ("x", 2)
m.set ("y", 3)
let res1 = m.run "add1 x y"
let res2 = m.run "add2 x y"

assert (res1 = (Value.Num 5))
assert (res2 = (Value.Num 5))

// Native calling Mana

m.run "let addM = {|x, y| x + y}"

let _ = m.call ("addM", [ Value.Num(2); Value.Num(3) ]) // Manual call

let addM = m.get<int -> int -> int> "addM" // Automatic native conversion
let resM = addM 2 3

assert (resM = 5)
