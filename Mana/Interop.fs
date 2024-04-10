module Mana.Interop

open System
open System.Collections
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Reflection
open Mana
open Yute.Reflection

let rec (|FSharpList|_|) (env: Env<Value>) (obj: obj) =
    match obj with
    | :? IEnumerable as enumerable when FSharpType.isList (obj.GetType()) ->
        Some(
            enumerable
            |> Seq.cast<obj>
            |> Seq.map (fromNative env)
            |> Seq.toList
        )
    | _ -> None

and (|FSharpArray|_|) (env: Env<Value>) (obj: obj) =
    match obj with
    | :? Array as array ->
        Some(
            array
            |> Seq.cast<obj>
            |> Seq.map (fromNative env)
            |> Seq.toList
        )
    | _ -> None

and (|FSharpRecord|_|) (env: Env<Value>) (obj: obj) =
    let isRecordType typ =
        FSharpType.IsRecord(typ, BindingFlags.Public ||| BindingFlags.NonPublic)

    let typeOfObj = obj.GetType()

    if isRecordType typeOfObj then
        let fields =
            FSharpType.GetRecordFields(typeOfObj, BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.map (fun fieldInfo ->
                KeyValuePair(Value.Str(fieldInfo.Name), fromNative env (fieldInfo.GetValue(obj)))
            )

        Some(Dictionary(fields))
    else
        None

and (|FSharpUnion|_|) (obj: obj) =

    let onlyEmptyUnionCases typ =
        (FSharpType.GetUnionCases(typ, BindingFlags.Public ||| BindingFlags.NonPublic)
         |> Array.forall (fun uc -> uc.GetFields().Length = 0))

    let isUnionType typ =
        FSharpType.IsUnion(typ, BindingFlags.Public ||| BindingFlags.NonPublic)

    let typeOfObj = obj.GetType()

    if isUnionType typeOfObj && onlyEmptyUnionCases typeOfObj then
        let unionCaseInfo, _ = FSharpValue.GetUnionFields(obj, typeOfObj)
        Some unionCaseInfo.Name
    else
        None

and (|FSharpSome|_|) (env: Env<Value>) (obj: obj) =
    if FSharpValue.isSome obj then
        FSharpValue.getSomeValue obj |> fromNative env |> Some
    else
        None

and (|FSharpFunction|_|) (env: Env<Value>) (obj: obj) =
    let t = obj.GetType()

    if not <| FSharpType.IsFunction t then
        None
    else
        Some <| fromNativeFunction env "<native>" obj

and fromNativeFunction (env: Env<Value>) (name: string) (obj: obj) =
    let t = obj.GetType()

    let nativeName = t.Name.Split("@")[0]

    if not <| FSharpType.IsFunction t then
        failwith $"obj `${nativeName}` is not a function"

    let inputs, _ = FSharpType.getFunctionType t

    let rec typeCheck (env: Env<Value>) (values: Value list) (types: Type list) : obj list =
        match types with
        | [] ->
            if values.Length > 0 then
                failwith $"Too many arguments for function `${nativeName}`"
            else
                []
        | t :: restTypes when t = typeof<Env<Value>> -> box env :: typeCheck env values restTypes
        | t :: restTypes ->
            match values with
            | [] -> failwith $"Not enough arguments for function `${nativeName}`"
            | v :: restValues ->
                (fromValueBoxed env t v)
                :: (typeCheck env restValues restTypes)

    let fn (env: Env<Value>) (values: Value list) : Value =

        let checkedValues = typeCheck env values inputs

        let ret = FSharpValue.dynamicInvoke obj checkedValues

        fromNative env ret

    fn

and fromNative (env: Env<Value>) (obj: obj) : Value =
    match obj with
    | :? Value -> obj :?> Value
    | :? unit -> Value.Nil // Also matches None
    | :? bool as b -> Value.Bool b
    | :? int as n -> Value.Num(float n)
    | :? float as n -> Value.Num n
    | :? string as s -> Value.Str s
    | FSharpList env items -> Value.List items
    | FSharpArray env items -> Value.List items
    | FSharpRecord env items -> items |> Seq.map (|KeyValue|) |> Map.ofSeq |> Value.Table
    | FSharpUnion caseName -> Value.Str caseName
    | FSharpSome env value -> value
    | FSharpFunction env fn -> Value.Closure(fn)
    | _ -> Value.Nil

and fromValueBoxed (env: Env<Value>) (t: Type) (v: Value) : obj =
    match t with
    | t when t = typeof<Value> -> box v

    | t when t = typeof<unit> ->
        match v with
        | Value.Nil -> box null
        | _ -> failwith "Type mismatch for unit"

    | t when t = typeof<bool> ->
        match v with
        | Value.Bool b -> box b
        | _ -> failwith "Type mismatch for bool"

    | t when t = typeof<float> ->
        match v with
        | Value.Num n -> box n
        | _ -> failwith "Type mismatch for float"

    | t when t = typeof<int> ->
        match v with
        | Value.Num n when n = float (int n) -> box (int n)
        | _ -> failwith "Type mismatch for int"

    | t when t = typeof<string> ->
        match v with
        | Value.Str s -> box s
        | _ -> failwith "Type mismatch for string"

    | t when FSharpType.isList t ->
        let elementsType = t.GenericTypeArguments[0]

        match v with
        | Value.List items ->

            items
            |> List.map (fromValueBoxed env elementsType)
            |> FSharpValue.makeList elementsType

        | _ -> failwith "Type mismatch for list"

    | t when FSharpType.isArray t ->
        let elementsType = t.GetElementType()

        match v with
        | Value.Nil -> FSharpValue.makeArray elementsType []
        | Value.List items ->
            let elementsType = t.GetElementType()

            items
            |> Seq.map (fromValueBoxed env elementsType)
            |> FSharpValue.makeArray elementsType
        | _ -> failwith "Type mismatch for Array"
    | t when FSharpType.isOption t ->
        let elementsType = t.GenericTypeArguments[0]

        match v with
        | Value.Nil -> FSharpValue.makeNone elementsType
        | v ->
            let boxedValue = fromValueBoxed env elementsType v
            FSharpValue.makeSome elementsType boxedValue
    | t when
        FSharpType.IsUnion(t, BindingFlags.Public ||| BindingFlags.NonPublic)
        && (FSharpType.GetUnionCases(t, BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.forall (fun uc -> uc.GetFields().Length = 0))
        ->
        match v with
        | Value.Str kw ->
            let unionCases =
                FSharpType.GetUnionCases(t, BindingFlags.Public ||| BindingFlags.NonPublic)

            let matchedCase =
                unionCases
                |> Array.tryFind (fun uc -> uc.Name = kw)
                |> function
                    | Some uc -> FSharpValue.MakeUnion(uc, [||], BindingFlags.Public ||| BindingFlags.NonPublic)
                    | None -> failwith "No matching union case found"

            box matchedCase
        | _ -> failwith "Type mismatch for discriminated union"

    | t when FSharpType.IsRecord(t, BindingFlags.Public ||| BindingFlags.NonPublic) ->
        match v with
        | Value.Table(table) ->
            let fields =
                FSharpType.GetRecordFields(t, BindingFlags.Public ||| BindingFlags.NonPublic)

            let fieldValues =
                fields
                |> Array.map (fun fi ->
                    let typ = fi.PropertyType

                    let key = Value.Str(fi.Name)
                    let fieldValue = Map.tryFind key table

                    match fieldValue with
                    | Some value -> fromValueBoxed env typ value
                    | None -> // Special cases for option, list and array
                        if FSharpType.isOption typ then
                            FSharpValue.makeNone typ.GenericTypeArguments[0]
                        elif FSharpType.isList typ then
                            FSharpValue.makeList typ.GenericTypeArguments[0] []
                        elif FSharpType.isArray typ then
                            FSharpValue.makeArray (typ.GetElementType()) []
                        else
                            failwith $"Key not found in Table: {fi.Name}"
                )

            box (FSharpValue.MakeRecord(t, fieldValues, BindingFlags.Public ||| BindingFlags.NonPublic))
        | _ -> failwith "Type mismatch for Record"
    | t when FSharpType.IsFunction t ->
        match v with
        | Value.Closure(handler) ->

            let rec makeFn (fnType: Type) (args: Value list) : obj =
                let _, output = FSharpType.GetFunctionElements fnType
                let isLast = not (FSharpType.IsFunction output)

                let fn: obj -> obj =
                    fun i ->
                        let args = args @ [ i |> fromNative env ]

                        if isLast then
                            handler env args |> fromValueBoxed env output
                        else
                            makeFn output args

                FSharpValue.MakeFunction(fnType, fn)

            makeFn t []

        | _ -> failwith "Type mismatch for Function"
    | _ -> failwith $"Unsupported native type for conversion: {t}"

and fromValue<'T> (env: Env<Value>) (v: Value) : 'T =
    let boxedValue = fromValueBoxed env typeof<'T> v
    unbox<'T> boxedValue
