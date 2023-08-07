namespace Mana

type CompileError = | Todo

type CompileResult<'t> = Result<'t, CompileError>

type RuntimeError =
    | InvalidArguments
    | UnknownIdent
    | FunctionNotFound
    | NotAFunction

type RuntimeResult<'t> = Result<'t, RuntimeError>
