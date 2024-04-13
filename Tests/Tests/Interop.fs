module Tests.Interop

open Mana
open Tests.Testing

type Bar =
    | Alpha
    | Beta

type Foo = {
    a: int
    b: bool
    c: string list
    d: float array list
    e: Bar
    absentList: List<int>
    absentArray: int[]
    absentOption: Option<int>
}

let nativeFunction (_: Env<Value>) (x: int) (s: string) : int = x + s.Length

let fsharp2mana (o: obj) expected =
    let m = Mana()

    let result = o |> m.toValue
    let got = Value.repr result
    Test.equal got expected

let inline mana2fsharp (input: string) (expected: 'T) =

    let m = Mana()

    let value = m.run input

    let got: 'T = m.fromValue<'T> value

    if got <> expected then
        Test.fail "different"

let fromNative =
    let inline (==) o expected = fsharp2mana o expected

    testGroup "F# -> Mana" [
        test "int" { 1 == "1" }
        test "float" { 1.0 == "1" }
        test "string" { "a" == "\"a\"" }
        test "list" { [ 1; 2 ] == "[1, 2]" }
        test "array" { [| 1; 2 |] == "[1, 2]" }
        test "array of lists" { [| [ 1 ]; [ 2 ] |] == "[[1], [2]]" }
        test "option none" { None == "nil" }
        test "option some" { Some 1 == "1" }
        test "record" {
            let o: Foo = {
                a = 42
                b = false
                c = [ "hello"; "world" ]
                d = [ [| 3.14 |] ]
                e = Bar.Beta
                absentList = []
                absentOption = None
                absentArray = [||]
            }

            let expected =
                "#[\"a\": 42, \"absentArray\": [], \"absentList\": [], \"absentOption\": nil, \"b\": false, \"c\": [\"hello\", \"world\"], \"d\": [[3.14]], \"e\": \"Beta\"]"

            fsharp2mana o expected
        }

        test "function" {

            let m = Mana()

            m.set ("f", m.toValue nativeFunction)
            let result = m.run "f 4 \"hello\""

            let got = Value.repr result

            let expected = "9"

            Test.equal got expected
        }
    ]

let fromRune =

    let inline (==) o expected = mana2fsharp o expected

    testGroup "Mana -> F#" [
        test "int" { "1" == 1 }
        test "list" { "[1, 2, 3]" == [ 1; 2; 3 ] }
        test "array" { "[1, 2, 3]" == [| 1; 2; 3 |] }
        test "option none" { "nil" == None }
        test "option some" { "1" == Some 1 }
        test "record" {
            let input =
                "#[\"a\": 42, \"b\": false, \"c\": [\"hello\", \"world\"], \"d\": [[3.14]], \"e\": \"Beta\"]"

            let expected: Foo = {
                a = 42
                b = false
                c = [ "hello"; "world" ]
                d = [ [| 3.14 |] ]
                e = Bar.Beta
                absentList = []
                absentOption = None
                absentArray = [||]
            }

            input == expected
        }
        test "function" {
            let m = Mana()

            let f = (m.run "{|x, y| x + y}" |> m.fromValue<int -> int -> int>)

            let got = f 2 3 |> string

            Test.equal got "5"
        }
    ]

let interopTests = testGroup "interop" [ fromNative; fromRune ]
