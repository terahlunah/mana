module Tests.Format

open System
open System.IO
open Yute.Testing
open Rune.Interpreter

let testFormat name =

    test name {
        let projectDirectory = Environment.CurrentDirectory

        let sourcePath = Path.Combine(projectDirectory, "Tests", "format", $"{name}.rune")

        let source = File.ReadAllText sourcePath

        let formatted =
            try
                source |> Formatter.format
            with e ->
                Test.fail e.Message

        Test.equal formatted source
    }

let formatTests =
    testGroup "format" [
        testFormat "primitives"
        testFormat "defn"
        testFormat "defmacro"
        testFormat "def"
        testFormat "call"
        testFormat "do"
        testFormat "let"
        testFormat "ifwhen"
        testFormat "try"
    ]
