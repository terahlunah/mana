namespace Mana

type CompileError = | Todo

type CompileResult<'t> = Result<'t, CompileError>

type RuntimeError =
    | InvalidArguments
    | InvalidArgumentCount
    | UnknownIdent
    | FunctionNotFound
    | NotAFunction

type RuntimeResult<'t> = Result<'t, RuntimeError>
