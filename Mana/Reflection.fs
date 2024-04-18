namespace Mana

open System
open System.Reflection
open Microsoft.FSharp.Reflection

module CSharpType =
    
    let isAction (typ: Type) =
        let actionTypes =
            typeof<Action> ::
            ([
                
                typeof<Action<_>>
                typeof<Action<_, _>>
                typeof<Action<_, _, _>>
                typeof<Action<_, _, _, _>>
                typeof<Action<_, _, _, _, _>>
            ] |> List.map _.GetGenericTypeDefinition())
    
        typ.IsGenericType && List.exists ((=) (typ.GetGenericTypeDefinition())) actionTypes
    
    let isFunc (typ: Type) =
        let funcTypes =
            [
                typeof<Func<_>>
                typeof<Func<_, _>>
                typeof<Func<_, _, _>>
                typeof<Func<_, _, _, _>>
                typeof<Func<_, _, _, _, _>>
                typeof<Func<_, _, _, _, _, _>>
            ] |> List.map _.GetGenericTypeDefinition()

        
        typ.IsGenericType && List.exists ((=) (typ.GetGenericTypeDefinition())) funcTypes
    let isFunction (typ: Type) =
        isFunc typ || isAction typ
       

module FSharpType =
    let isOption (typ: Type) =
        typ.IsGenericType
        && typ.GetGenericTypeDefinition() = typedefof<option<_>>

    let isList (typ: Type) =
        typ.IsGenericType
        && typ.GetGenericTypeDefinition() = typedefof<list<_>>

    let isArray (typ: Type) = typ.IsArray

    let getFunctionType (typ: Type) =
        if not (FSharpType.IsFunction typ) then
            invalidArg "obj" "obj is not a Function"

        let rec functionPrototype t =
            if FSharpType.IsFunction t then
                let (input, output) = FSharpType.GetFunctionElements t
                let (inputs, ret) = functionPrototype output
                (input :: inputs, ret)
            else
                ([], t)

        functionPrototype typ

module FSharpValue =
    let isSome obj =
        let typeOfObj = obj.GetType()

        if FSharpType.IsUnion(typeOfObj, BindingFlags.Public ||| BindingFlags.NonPublic) then
            let unionCaseInfo, _ = FSharpValue.GetUnionFields(obj, typeOfObj)

            typeOfObj.IsGenericType
            && typeOfObj.GetGenericTypeDefinition() = typedefof<Option<_>>
            && unionCaseInfo.Name = "Some"

        else
            false

    let getSomeValue obj =
        if not (isSome obj) then
            invalidArg "obj" "obj is not an Option.Some"

        let _, values = FSharpValue.GetUnionFields(obj, obj.GetType())
        values[0]

    let isNone obj =
        let typeOfObj = obj.GetType()

        if FSharpType.IsUnion(typeOfObj, BindingFlags.Public ||| BindingFlags.NonPublic) then
            let unionCaseInfo, _ = FSharpValue.GetUnionFields(obj, typeOfObj)

            typeOfObj.IsGenericType
            && typeOfObj.GetGenericTypeDefinition() = typedefof<Option<_>>
            && unionCaseInfo.Name = "None"
        else
            false

    let makeSome (elementType: Type) (value: obj) : obj =
        let optionType = typedefof<Option<_>>.MakeGenericType([| elementType |])

        let someConstructor =
            FSharpType.GetUnionCases(optionType)
            |> Array.find (fun uc -> uc.Name = "Some")

        FSharpValue.MakeUnion(someConstructor, [| value |])

    let makeNone (elementType: Type) : obj =
        let optionType = typedefof<Option<_>>.MakeGenericType([| elementType |])

        let noneConstructor =
            FSharpType.GetUnionCases(optionType)
            |> Array.find (fun uc -> uc.Name = "None")

        FSharpValue.MakeUnion(noneConstructor, [||])

    let makeList (elementType: Type) (items: seq<obj>) : obj =
        let listType = typedefof<list<_>>.MakeGenericType([| elementType |])

        let consConstructor =
            FSharpType.GetUnionCases(listType)
            |> Array.find (fun uc -> uc.Name = "Cons")

        let emptyConstructor =
            FSharpType.GetUnionCases(listType)
            |> Array.find (fun uc -> uc.Name = "Empty")

        let emptyValue = FSharpValue.MakeUnion(emptyConstructor, [||])

        let mutable tempList = emptyValue

        for boxedValue in Seq.rev items do
            let consValue = FSharpValue.MakeUnion(consConstructor, [| boxedValue; tempList |])
            tempList <- consValue

        tempList

    let makeArray (elementType: Type) (items: seq<obj>) : obj =
        let arr = Array.CreateInstance(elementType, Seq.length items)

        items |> Seq.iteri (fun i item -> arr.SetValue(item, i))

        box arr

    let dynamicInvoke (f: obj) (args: obj seq) =
        let rec dynamicFunctionInternal (next: obj) (args: obj list) =
            match args.IsEmpty with
            | false ->
                let fType = next.GetType()

                if FSharpType.IsFunction fType then
                    let (head, tail) = (args.Head, args.Tail)

                    let methodInfo =
                        fType.GetMethods()
                        |> Seq.filter (fun x -> x.Name = "Invoke" && x.GetParameters().Length = 1)
                        |> Seq.head

                    let partalResult = methodInfo.Invoke(next, [| head |])
                    dynamicFunctionInternal partalResult tail
                else
                    invalidArg "obj" "obj is not a Function"
            | true -> next

        dynamicFunctionInternal f (args |> List.ofSeq)
