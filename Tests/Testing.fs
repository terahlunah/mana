module Tests.Testing

open System
open DiffPlex.DiffBuilder
open DiffPlex.DiffBuilder.Model
open Tests.Console

type AssertKind = Equals of msg: string * actual: string * expected: string

exception AssertException of kind: AssertKind
exception FailedException of message: string

let inline printLevel level =
    if level = 0 then
        printfn
    else
        let indent = String.replicate (level - 1) "  "
        (printf $"{indent}▶ ")
        printfn

type FailedDetails =
    | Message of msg: string
    | Diff of name: string * diff: DiffPaneModel

type TestResult =
    | Failed of name: string * details: FailedDetails
    | Passed of name: string
    | Group of name: string * tests: TestResult list

module TestResult =
    let rec passed r =
        match r with
        | Passed _ -> true
        | Failed _ -> false
        | Group(_, tests) -> tests |> List.forall passed

    let rec passedCount r =
        match r with
        | Passed _ -> 1
        | Failed _ -> 0
        | Group(_, tests) -> tests |> List.sumBy passedCount

    let rec failedCount r =
        match r with
        | Passed _ -> 0
        | Failed _ -> 1
        | Group(_, tests) -> tests |> List.sumBy failedCount

    let rec totalCount r =
        match r with
        | Passed _ -> 1
        | Failed _ -> 1
        | Group(_, tests) -> tests |> List.sumBy totalCount

type Test =
    | Single of name: string * code: (unit -> unit)
    | Group of name: string * tests: Test list

type TestSingleBuilder(name) =
    member _.TryFinally(f, compensation) =
        try
            f ()
        finally
            compensation ()

    member _.TryWith(f, catchHandler) =
        try
            f ()
        with e ->
            catchHandler e

    member _.Using(disposable: #IDisposable, f) = using disposable f

    member _.For(sequence, f) =
        for i in sequence do
            f i

    member _.While(gd, prog) =
        while gd () do
            prog ()

    member _.Combine(f1, f2) =
        f2 ()
        f1

    member _.Zero() = ()
    member _.Delay f = f

    member _.Run f = Single(name, f)

let inline test name = TestSingleBuilder name
let inline testGroup name tests = Group(name, tests)

module Test =
    let rec count tests =
        tests
        |> List.sumBy (fun test ->
            match test with
            | Single _ -> 1
            | Group(_, tests) -> tests |> count
        )

    let filtered (f: string) tests =
        let rec filterInner f prefix tests =
            tests
            |> List.choose (fun test ->
                match test with
                | Single(name, _) ->
                    let fullName = $"{prefix}{name}"
                    if fullName.Contains(f: string) then Some(test) else None
                | Group(name, tests) ->
                    let filteredTests = filterInner f $"{prefix}{name}/" tests

                    if List.isEmpty filteredTests then
                        None
                    else
                        Group(name, filteredTests) |> Some

            )

        filterInner f "" tests

    let inline fail msg = msg |> FailedException |> raise

    let equal actual expected =
        if actual = expected then
            ()
        else
            AssertKind.Equals("", actual, expected)
            |> AssertException
            |> raise

    let equalMessage msg actual expected =
        if actual = expected then
            ()
        else
            AssertKind.Equals(msg, actual, expected)
            |> AssertException
            |> raise

    let name (test: Test) =
        match test with
        | Single(name, _) -> name
        | Group(name, _) -> name

    let rec run level (test: Test) : TestResult =
        match test with
        | Single(name, code) ->
            let result =
                try
                    code ()
                    TestResult.Passed name
                with
                | AssertException assertKind ->
                    match assertKind with
                    | Equals(msg, actual, expected) ->
                        let diff = InlineDiffBuilder.Diff(actual, expected, ignoreWhiteSpace = false)
                        let details = FailedDetails.Diff(msg, diff)

                        TestResult.Failed(name, details)
                | FailedException msg ->
                    let msg = FailedDetails.Message msg
                    TestResult.Failed(name, msg)
                | e ->
                    let msg = FailedDetails.Message e.Message
                    TestResult.Failed(name, msg)

            let color =
                if TestResult.passed result then
                    ConsoleColor.Green
                else
                    ConsoleColor.Red

            colored color { printLevel level $"{name}" }

            result
        | Group(name, tests) ->
            printLevel level $"{name}"

            let results = tests |> List.map (run (level + 1))

            TestResult.Group(name, results)

    let rec reportFailures prefix (test: TestResult) =
        match test with
        | TestResult.Failed(name, failedDetails) ->
            colored ConsoleColor.Red { printfn $"▼ {prefix}{name}" }
            printfn "------------------------------"

            match failedDetails with
            | Message s -> colored ConsoleColor.Red { printfn $"Error: {s}" }
            | Diff(msg, diff) ->
                if msg <> "" then
                    printfn $"# {msg}"

                for line in diff.Lines do
                    match line.Type with
                    | ChangeType.Deleted -> colored ConsoleColor.Green { printfn $"+ {line.Text}" }
                    | ChangeType.Inserted -> colored ConsoleColor.Red { printfn $"- {line.Text}" }
                    | _ -> printfn $"  {line.Text}"

            printfn "------------------------------"
        | TestResult.Passed _ -> ()
        | TestResult.Group(name, testResults) ->
            for test in testResults do
                reportFailures $"{prefix}{name}/" test

    let rec runAll filter (tests: Test list) =
        let totalTestsCount = count tests
        let tests = filtered filter tests
        let ignoredCount = totalTestsCount - count tests

        let allTests = testGroup "========= All Tests  =========" tests
        let results = run 0 allTests

        let passedCount = TestResult.passedCount results
        let failedCount = TestResult.failedCount results
        let totalCount = TestResult.totalCount results

        if failedCount <> 0 then
            printfn "========== Failures =========="

            match results with
            | TestResult.Group(_, tests) -> tests |> List.iter (reportFailures "")
            | _ -> failwith "unreachable"

        printfn "========== Summary ==========="
        colored ConsoleColor.DarkGray { printfn $"Ignored : {ignoredCount}" }
        colored ConsoleColor.Green { printfn $"Passed  : {passedCount}" }
        colored ConsoleColor.Red { printfn $"Failed  : {failedCount}" }
        printfn $"Total   : {totalCount}"
        printfn ""
