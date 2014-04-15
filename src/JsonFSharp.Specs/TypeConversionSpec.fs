module TypeConversionSpec
open System.Text.RegularExpressions
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open JsonParser

type FooTypeWithString = { foo : string; }
type FooTypeWithInt = { foo : int; }
type ParentType = {
    child: FooTypeWithInt;
    bar: int
    }

let getSuccess = function
    | Success(x) -> x
    | Failure(x) -> failwith x

let stringToJson = JsonInput.fromString >> parse >> getSuccess
let jsonToObj<'T> = toInstance<'T> >> getSuccess
let getFailure value = 
    match value with
    | Success(_) -> failwith "Expected failure, was success"
    | Failure(x) -> x

let specs = 
    describe "Type conversions" [
        describe "Simple type conversions" [
            it "an initialize string values" <| fun () ->
                let value = 
                    """{ "foo": "bar" }"""      
                    |> stringToJson
                    |> jsonToObj<FooTypeWithString>
                value.foo |> should equal "bar"
            it "can initialize integer values" <| fun () ->
                let value = 
                    """{ "foo": 42 }"""      
                    |> stringToJson
                    |> jsonToObj<FooTypeWithInt>
                value.foo |> should equal 42
        ]

        it "should return parent type" <| fun () ->
            let value =
                """{ "child": { "foo": 42 }, "bar": 43 }"""
                |> stringToJson
                |> jsonToObj<ParentType>
            value.child.foo |> should equal 42
            value.bar |> should equal 43

        describe "type mismatch" [
            describe "when json does not contain correct parameter" [
                it "should return a proper error type" <| fun () ->
                    """{ "bar": 42 }"""
                    |> stringToJson
                    |> toInstance<FooTypeWithInt>
                    |> getFailure
                    |> should matchRegex "could not find data for record value 'foo'"
            ]
            describe "when json data and record value are incompatible" [
                it "should return a proper error type" <| fun () ->
                    """{ "foo": "bar" }"""
                    |> stringToJson
                    |> toInstance<FooTypeWithInt>
                    |> getFailure
                    |> should matchRegex "incompatible type"
            ]
        ]
    ]