namespace Mana

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
            handler = "__neg"
            precedence = 40
        }
        {
            symbol = "!"
            handler = "__not"
            precedence = 40
        }
    ]

    let find symbol =
        unary |> List.tryFind (fun op -> op.symbol = symbol)

module BinaryOperator =
    let binary = [
        {
            symbol = "+"
            handler = "__add"
            precedence = 50
            associativity = Left
        }
        {
            symbol = "-"
            handler = "__sub"
            precedence = 50
            associativity = Left
        }
        {
            symbol = "*"
            handler = "__mul"
            precedence = 60
            associativity = Left
        }
        {
            symbol = "/"
            handler = "__div"
            precedence = 60
            associativity = Left
        }
        {
            symbol = "**"
            handler = "__pow"
            precedence = 60
            associativity = Left
        }
        {
            symbol = "%"
            handler = "__mod"
            precedence = 60
            associativity = Left
        }
        {
            symbol = ">"
            handler = "__gt"
            precedence = 40
            associativity = Left
        }
        {
            symbol = ">="
            handler = "__ge"
            precedence = 40
            associativity = Left
        }
        {
            symbol = "<"
            handler = "__lt"
            precedence = 40
            associativity = Left
        }
        {
            symbol = "<="
            handler = "__le"
            precedence = 40
            associativity = Left
        }
        {
            symbol = "=="
            handler = "__eq"
            precedence = 40
            associativity = Left
        }
        {
            symbol = "!="
            handler = "__ne"
            precedence = 40
            associativity = Left
        }
        {
            symbol = "&&"
            handler = "__and"
            precedence = 30
            associativity = Left
        }
        {
            symbol = "||"
            handler = "__or"
            precedence = 20
            associativity = Left
        }
        {
            symbol = "."
            handler = "__chain"
            precedence = 70
            associativity = Left
        }
        {
            symbol = "++"
            handler = "concat"
            precedence = 10
            associativity = Left
        }
    ]

    let find symbol =
        binary |> List.tryFind (fun op -> op.symbol = symbol)
