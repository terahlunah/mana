namespace Mana

type CompileError = | Todo

type CompileResult<'t> = Result<'t, CompileError>

type RuntimeError =
    | InvalidArguments
    | InvalidArgumentCount
    | UnknownIdent
    | FunctionNotFound of name: string
    | NotAFunction
    | PatternMatchingFailed

type RuntimeResult<'t> = Result<'t, RuntimeError>
