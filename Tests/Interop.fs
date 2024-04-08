module Tests.Interop

open Rune.Interop
open Rune.Interpreter
open Rune
open Yute.Testing

type Bar =
    | Alpha
    | Beta

type Foo = {
    a: int
    b: bool
    c: string list
    d: float array list
    e: Interop.Symbol
    f: Bar
    absentList: List<int>
    absentArray: int[]
    absentOption: Option<int>
}

[<Name("runeFunction")>]
let nativeFunction (_: Env<Value>) (x: int) (_: string) (Rest(rest)) : int = x + rest.Length

let fsharp2rune (o: obj) expected =
    let env = Rune.env ()

    let result = o |> Interop.fromNative env
    let got = result.Repr()
    Test.equal got expected

let inline rune2fsharp (input: string) (expected: 'T) =
    let env = Rune.env ()

    let value = Reader.read env input

    let got: 'T = Interop.fromValue<'T> env value

    if got <> expected then
        Test.fail "different"

let fromNative =
    let inline (==) o expected = fsharp2rune o expected

    testGroup "F# -> Rune" [
        test "int" { 1 == "1" }
        test "float" { 1.0 == "1" }
        test "string" { "a" == "\"a\"" }
        test "list" { [ 1; 2 ] == "(1 2)" }
        test "array" { [| 1; 2 |] == "[1 2]" }
        test "array of lists" { [| [ 1 ]; [ 2 ] |] == "[(1) (2)]" }
        test "option none" { None == "nil" }
        test "option some" { Some 1 == "1" }
        test "record" {
            let env = Rune.env ()

            let o: Foo = {
                a = 42
                b = false
                c = [ "hello"; "world" ]
                d = [ [| 3.14 |] ]
                e = Interop.symbol env "test"
                f = Bar.Beta
                absentList = []
                absentOption = None
                absentArray = [||]
            }

            let result = o |> Interop.fromNative env
            let got = result.Repr()

            let expected =
                "{:a 42, :b false, :c (\"hello\" \"world\"), :d ([3.14]), :e test, :f :Beta, :absentList (), :absentArray [], :absentOption nil}"

            Test.equal got expected
        }

        test "function" {

            let env = Rune.env ()

            // fromNative env nativeFunction
            // let f = Interop.fromNative env nativeFunction

            env.set ("f", fromNative env nativeFunction)
            let result = Rune.eval "(f 4 \"hello\" 1 2 3)" env

            let got = result.Repr()

            let expected = "7"

            Test.equal got expected
        }
    ]

let fromRune =

    let inline (==) o expected = rune2fsharp o expected

    testGroup "Rune -> F#" [
        test "int" { "1" == 1 }
        test "list" { "(1 2 3)" == [ 1; 2; 3 ] }
        test "array" { "[1 2 3]" == [| 1; 2; 3 |] }
        test "option none" { "nil" == None }
        test "option some" { "1" == Some 1 }
        test "record" {
            let env = Rune.env ()

            let input =
                "{:a 42, :b false, :c (\"hello\" \"world\"), :d ([3.14]), :e test, :f :Beta}"

            let expected: Foo = {
                a = 42
                b = false
                c = [ "hello"; "world" ]
                d = [ [| 3.14 |] ]
                e = Interop.symbol env "test"
                f = Bar.Beta
                absentList = []
                absentOption = None
                absentArray = [||]
            }

            rune2fsharp input expected
        }
        test "function" {
            let env = Rune.env ()

            let f =
                Rune.eval "(fn [x y] (+ x y))" env
                |> Interop.fromValue<int -> int -> int> env

            let got = f 2 3 |> string

            Test.equal got "5"
        }
    ]

let interopTests = testGroup "interop" [ fromNative; fromRune ]
