module TypeConversionSpec
open System.Text.RegularExpressions
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open JsonParser

type FooTypeWithString = { foo : string; }
type FooTypeWithInt = { foo : int; }
type FooTypeWithIntOption = { foo : int option; }
type FooTypeWithBools = { t: bool; f: bool }
type FooTypeWithIntList = { foo: int list }
type ParentType = {
    child: FooTypeWithInt;
    bar: int
    }

type ParentWithList = {
    children : FooTypeWithInt list;
    bar : int }

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
            it "can initialize boolean values" <| fun () ->
                let value =
                    """{ "t": true, "f": false }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithBools>
                value.t |> should equal true
                value.f |> should equal false
        ]
        describe "null values" [
            it "Converts null to Option.None for option types" <| fun () ->
                let value =
                    """{ "foo": null }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithIntOption>
                value.foo |> should equal None

            it "Fails when type is not an option type" <| fun () ->
                let value = 
                    """{ "foo": null }"""
                    |> stringToJson
                    |> toInstance<FooTypeWithInt>
                match value with
                | Success _ -> failwith "Fail was expected"
                | Failure _ -> ()
        ]

        describe "array values" [
            it "converts the data to a compatible list" <| fun () ->
                let value =
                    """{ "foo": [1, 2] }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithIntList>
                value.foo |> should equal [1; 2]

            it "converts arrays with objects" <| fun () ->
                let value =
                    """{ "children": [ {"foo": 1}, {"foo": 2} ], "bar": 3}"""
                    |> stringToJson
                    |> jsonToObj<ParentWithList>
                let expected = { children=[{foo= 1};{foo= 2}];bar= 3}
                value |> should equal expected
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