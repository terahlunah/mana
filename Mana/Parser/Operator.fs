namespace Mana.Parser

type Associativity =
    | Left
    | Right

type BinaryOperator = {
    symbol: string
    handler: string
    precedence: int
    associativity: Associativity
}

type UnaryOperator = {
    symbol: string
    handler: string
    precedence: int
}

module UnaryOperator =
    let unary = [
        {
            symbol = "-"
            handler = "neg"
            precedence = 1
        }
    ]

    let find symbol =
        unary |> List.tryFind (fun op -> op.symbol = symbol)

module BinaryOperator =
    let binary = [
        {
            symbol = "+"
            handler = "add"
            precedence = 0
            associativity = Left
        }
        {
            symbol = "-"
            handler = "sub"
            precedence = 0
            associativity = Left
        }
        {
            symbol = "*"
            handler = "mul"
            precedence = 2
            associativity = Left
        }
        {
            symbol = "/"
            handler = "div"
            precedence = 2
            associativity = Left
        }
    ]

    let find symbol =
        binary |> List.tryFind (fun op -> op.symbol = symbol)
