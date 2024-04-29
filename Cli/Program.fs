open System
open Mana.Error
open PrettyPrompt
open PrettyPrompt.Highlighting
open Mana
open System.IO

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

let run path args =
    try
        let m = Mana()
        m.setValue ("args", args |> List.map Value.Str |> Value.List)

        let code = File.ReadAllText path

        let output = m.run code |> Value.repr

        printfn $"%s{output}"

    with :? ManaException as e ->
        printfn $"Error: {e.error}"

let args = Environment.GetCommandLineArgs() |> Array.skip 1

let readAllStdInText () =
    let rec readAllStdIn (lines: string list) =
        let line = Console.ReadLine()

        match line with
        | null -> String.concat Environment.NewLine (List.rev lines)
        | _ -> readAllStdIn (line :: lines)

    readAllStdIn []

match Seq.toList args with
| [] -> repl ()
| file :: args -> run file args
