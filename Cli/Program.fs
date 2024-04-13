open System
open Mana.Error
open PrettyPrompt
open PrettyPrompt.Highlighting
open Mana
open System.IO

open CommandLine
open CommandLine.Text

type CliArgs = {
    [<Option('f', "format", HelpText = "Format a file.")>]
    format: bool
    [<Option('i', "format-stdin", HelpText = "Format stdin input.")>]
    stdin: bool
    [<Value(0, MetaName = "file", HelpText = "A file to run or format.")>]
    file: string option
} with

    [<Usage>]
    static member examples = seq {
        yield
            Example(
                "Start the repl",
                {
                    format = false
                    stdin = false
                    file = None
                }
            )

        yield
            Example(
                "Run a file",
                {
                    format = false
                    stdin = false
                    file = "test.mana" |> Some
                }
            )

        yield
            Example(
                "Format a file",
                {
                    format = true
                    stdin = false
                    file = "test.mana" |> Some
                }
            )
    }

let repl () =
    let config = PromptConfiguration(prompt = FormattedString("mana> "))
    let prompt = Prompt(configuration = config)

    let m = Mana()

    let rec loop () =
        let input =
            prompt.ReadLineAsync()
            |> Async.AwaitTask
            |> Async.RunSynchronously

        if input.IsSuccess then
            try
                m.run input.Text |> Value.repr |> printfn "%s"
            with ManaException e ->
                printfn $"Error: %A{e}"

            loop ()

    loop ()

let run path argv =
    try
        let m = Mana()
        m.set ("argv", argv |> List.map Value.Str |> Value.List)

        let code = File.ReadAllText path

        let output = m.run code |> Value.repr

        printfn $"%s{output}"

        0
    with :? ManaException as e ->
        printfn $"Error: {e.error}"
        1

let format source =
    // Formatter.format source |> display
    0

let inline (|Success|Help|Version|Fail|) (result: ParserResult<'a>) =
    match result with
    | :? Parsed<'a> as parsed -> Success(parsed.Value)
    | :? NotParsed<'a> as notParsed when notParsed.Errors.IsHelp() -> Help
    | :? NotParsed<'a> as notParsed when notParsed.Errors.IsVersion() -> Version
    | :? NotParsed<'a> as notParsed -> Fail(notParsed.Errors)
    | _ -> failwith "invalid parser result"

let args = Environment.GetCommandLineArgs() |> Array.skip 1
let result = Parser.Default.ParseArguments<CliArgs>(args)

let readAllStdInText () =
    let rec readAllStdIn (lines: string list) =
        let line = Console.ReadLine()

        match line with
        | null -> String.concat Environment.NewLine (List.rev lines)
        | _ -> readAllStdIn (line :: lines)

    readAllStdIn []

match result with
| Success(opts) ->
    if opts.stdin then
        let source = readAllStdInText ()
        source |> format |> exit
    elif opts.format then
        match opts.file with
        | Some file -> file |> File.ReadAllText |> format |> exit
        | None -> exit 1
    else
        match opts.file with
        | None ->
            repl ()
            exit 0
        | Some file -> run file [] |> exit
| Fail _ -> exit 1
| Help
| Version -> exit 0
