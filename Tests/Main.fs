open System

open Yute.Testing
open Tests.Parser
open Tests.Optimizer
open Tests.Interop

let args = Environment.GetCommandLineArgs()

let filter = if args.Length > 1 then args[1] else ""

Test.runAll filter [ parserTests; optimizerTests; interopTests ]
