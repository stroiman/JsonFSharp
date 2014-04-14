module TypeConversionSpec
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open JsonParser
open System.Linq

type FooType = {
    foo : string;
    foo2: int
    }

type ChildType = {
    foo: int
    }

type ParentType = {
    child: ChildType;
    bar: int
    }

let getJson = function
    | Success(x) -> x
    | Failure(x) -> failwith x

let getObjectData (json: JsonValue) =
    match json with
    | JsonString(x) -> x :> System.Object
    | JsonNumber(x) -> x :> System.Object
    | _ -> failwith "Not implemented"

let rec toInstanceOfType (targetType: System.Type) (json: JsonValue) =
    match json with
    | JsonObject(obj) ->
        let getValue name = obj.[name]
        let ctor = targetType.GetConstructors().Single()
        let ctorParameters = 
            ctor.GetParameters() 
            |> Array.toList
            |> List.map (fun param -> 
                let jsonValue = getValue param.Name
                match jsonValue with
                | JsonObject(x) -> toInstanceOfType param.ParameterType jsonValue
                | _ ->
                    let value = jsonValue |> getObjectData
                    System.Convert.ChangeType(value, param.ParameterType))
            |> List.toArray
        ctor.Invoke(ctorParameters)
    | _ -> failwith "Not an object"

let toInstance<'T> (json: JsonValue) =
    let targetType = typeof<'T>
    toInstanceOfType targetType json :?> 'T

let specs = 
    describe "Type conversions" [
        it "should return instance of specified type" <| fun () ->
            let value = 
                """{ "foo": "bar", "foo2": 42 }"""      
                |> JsonInput.fromString
                |> parse
                |> getJson
                |> toInstance<FooType>
            value.foo |> should equal "bar"

        it "should return parent type" <| fun () ->
            let value =
                """{ "child": { "foo": 42 }, "bar": 43 }"""
                |> JsonInput.fromString
                |> parse
                |> getJson
                |> toInstance<ParentType>
            value.child.foo |> should equal 42
            value.bar |> should equal 43
                
    ]