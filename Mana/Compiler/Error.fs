namespace Mana

type CompileError = | Todo

type RuntimeError =
    | InvalidArguments
    | InvalidArgumentCount
    | UnknownIdent
    | FunctionNotFound of name: string
    | NotAFunction
    | PatternMatchingFailed

type CompileResult<'T> = Result<'T, CompileError>
type RuntimeResult<'T> = Result<'T, RuntimeError>
