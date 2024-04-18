# Mana

An experimental minimalist yet expressive scripting language for dotnet programs.

While meant to be embedded in a host program, it comes with a simple REPL.

Mana has been created to serve as the scripting language for my private generative art tools.
Hence, the language design is trying to balance the following properties:

- ### Minimalist
  - Simple to parse and run.
  - Contain the minimum it needs to be useful and delegate as needed to the host.
  - Light and consistent syntax.
- ### Expressive
  - Primitives should be powerful enough to support most use cases out of the box.
  - Changing behavior should only need minimal code changes.
- ### Intuitive
  - Try to use well known syntax.
  - Core functions should behave as you'd expect.

#### These properties aim to create a tight feedback loop so one can focus on the art and not the script.

# Features

- [x] Primitives (Nil, Bool, Num, Str, List, Table, Functions)
- [x] First class functions
- [x] Implicit `it` (Kotlin style)
- [x] Chain call syntax `x.foo.bar.print` (data first pipeline)
- [x] Pattern matching
  - [x] Primitives
  - [x] List destructuring
  - [ ] Table destructuring
- [x] Builtin core functions
  - [ ] for/range loop construct
  - [ ] Documentation
- [ ] Early return / Loop break
- [ ] Imports
- [ ] Error reporting
- [ ] Reference values

# Language overview

---

## Comments

Comments in mana use the `;` symbol

`; This is a comment`


---

## Values

### Literals

There is 4 types if literals in Mana:

### Nil
With a single value `nil`
### Bool
With 2 values `true` and `false`
### Num
Numbers like `42` or `3.14`
### Str
Strings like `"Hello World"`

---

### Lists

A basic collection of values.

```
[1, 2, 3]
```

---

### Tables

Tables are dictionaries of `value:value` pairs. Any kind of value can serve as a key.

```
#[
  "a" : 1,
  "b" : 2
]
```

---

## Bindings

Values can be locally bound to symbols using `let` bindings.

```
let pi = 3.14
```

They can then be reused later (in the same scope)

```
let tau = 2 * pi
```

---

## Functions

Functions in Mana are all defined as lambdas/closures.
They can be passed around and bound to symbols like any other kind of value.

For example this is how you would define an `add` function.

```
let add = {|a, b| a + b } 
```

Functions can return other functions.

For functions with a single parameter, the shorthand `it` symbol can be used directly.

```
let adder = { |n|
  { it + n } 
}

let add2 = adder 2

add2 3 ; = 5
```

---

## Chain calls

The `.` operator allows to write a sequence of operation of a value in an ergonomic way.

It takes the expression on its left and insert it as the first parameter of the function call on its right.

These 2 snippets are equivalent:

```
head (rev (map [1,2,3] { it + 1})) ; = 4
```
```
[1,2,3].map { it + 1}.rev.head ; = 4
```

The chain syntax makes the sequences of operations left-to-right and clearer.

---

## Pattern matching

Pattern matching and destructuring is suported on `let` and `match` expressions

```
let [a, b] = [1, 2]

match [1, 2, 3]
| 0 -> display "not the right type"
| [] -> display "empty list"
| [head, ..tail] -> display "non empty list"

```

---

# Interop

The interpreter is written in `F#` but the language can be used from any `dotnet` language, including `C#`.

See the usage examples:
- [F# example](Example%20Fsharp/Program.fs)
- [C# example](Example%20Csharp/Program.cs)

---

# Implementation

It is composed of 3 simple and independent parts:
- Lexer (code -> tokens)
- Parser (tokens -> AST)
- Compiler (AST -> closure)

The AST is 'compiled' into a deeply nested closure. Execution simply evaluates the closure.