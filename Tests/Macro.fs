module Tests.Macro

open Rune.Interpreter
open Rune
open Yute.Testing

let testEval expr expected =
    let env = Rune.env ()

    // env.context.setTrace true

    let result = expr |> Reader.readMany env |> Evaluator.evalMany env
    let got = result.Repr()
    Test.equalMessage $"{expr}" got expected

let (==) expr expected = testEval expr expected

let quasiquote =
    testGroup "quasiquote" [

        test "simple quasiquote" { "`(1 2 3)" == "(1 2 3)" }

        test "unquote within quasiquote" { "`(1 ~(+ 1 1) 3)" == "(1 2 3)" }

        test "unquote-splicing within quasiquote" { "`(1 ~@(list 2 3) 4)" == "(1 2 3 4)" }

        test "unquote-splicing with empty list" { "`(1 ~@() 2)" == "(1 2)" }
    ]

let defmacro =
    testGroup "defmacro" [
        test "dummy" { "(defmacro dummy [x] 1) (dummy nil)" == "1" }
        test "pass" {
            "(defmacro identity [x] `'~x) (identity (+ 1 2))"
            == "(+ 1 2)"
        }
        test "delay" {
            """
            (defmacro delay [& body]
              `(let [called false
                     cache nil]
                 (fn []
                   (if called
                       cache
                       (do
                         (set! called true)
                         (set! cache ~@body)
                         cache)))))
            ((delay (+ 1 2)))    
            """
            == "3"
        }
    ]

let macroTests = testGroup "macro" [ quasiquote; defmacro ]
