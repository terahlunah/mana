module Mana.ResultState

type ResultState<'State, 'T, 'E> = ResultState of f: ('State -> Result<'T * 'State, 'E>)

// Unwrap the monad
// M<S, T, E> -> S -> Result<T*S, E>
let run (state: 'S) (m: ResultState<'S, 'T, 'E>) = let (ResultState f) = m in f state

// From normal world to monadic world: T -> M
let lift t : ResultState<'S, 'T, 'E> = ResultState(fun state -> Ok(t, state))
let liftError e : ResultState<'S, 'T, 'E> = ResultState(fun state -> Error e)

// M<S, T1, E> -> (S->M<S, T2, E>) -> M<S, T2, E>
let bind (m: ResultState<'S, 'T1, 'E>) (f: 'T1 -> ResultState<'S, 'T2, 'E>) : ResultState<'S, 'T2, 'E> =
    let m2 =
        fun state ->
            match run state m with
            | Ok(t, state2) -> run state2 (f t)
            | Error e -> Error e

    ResultState m2
