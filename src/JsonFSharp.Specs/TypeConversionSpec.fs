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

let getJson = function
    | Success(x) -> x
    | Failure(x) -> failwith x

let getObjectData (json: JsonValue) =
    match json with
    | JsonString(x) -> x :> System.Object
    | JsonNumber(x) -> x :> System.Object
    | _ -> failwith "Not implemented"

let toInstance<'T> (json: JsonValue) =
    match json with
    | JsonObject(obj) ->
        let getValue name = obj.[name]
        let targetType = typeof<'T>
        let ctor = targetType.GetConstructors().Single()
        let ctorParameters = 
            ctor.GetParameters() 
            |> Array.toList
            |> List.map (fun param -> 
                let value = getValue param.Name |> getObjectData
                System.Convert.ChangeType(value, param.ParameterType))
            |> List.toArray
        ctor.Invoke(ctorParameters) :?> 'T
    | _ -> failwith "Not an object"

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
    ]