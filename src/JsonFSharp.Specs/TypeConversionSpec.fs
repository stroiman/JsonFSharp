module TypeConversionSpec
open System.Text.RegularExpressions
open FSpec.Core.Dsl
open FSpec.Core.Matchers
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
type ParentTypeWithMap = {
        children: Map<string,FooTypeWithInt>
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
            it "an initialize string values" <| fun _ ->
                let value = 
                    """{ "foo": "bar" }"""      
                    |> stringToJson
                    |> jsonToObj<FooTypeWithString>
                value.foo |> should equal "bar"
            it "can initialize integer values" <| fun _ ->
                let value = 
                    """{ "foo": 42 }"""      
                    |> stringToJson
                    |> jsonToObj<FooTypeWithInt>
                value.foo |> should equal 42
            it "can initialize boolean values" <| fun _ ->
                let value =
                    """{ "t": true, "f": false }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithBools>
                value.t |> should equal true
                value.f |> should equal false
        ]
        describe "null values" [
            it "Converts null to Option.None for option types" <| fun _ ->
                let value =
                    """{ "foo": null }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithIntOption>
                value.foo |> should equal None

            it "Fails when type is not an option type" <| fun _ ->
                let value = 
                    """{ "foo": null }"""
                    |> stringToJson
                    |> toInstance<FooTypeWithInt>
                match value with
                | Success _ -> failwith "Fail was expected"
                | Failure _ -> ()
        ]

        describe "array values" [
            it "converts the data to a compatible list" <| fun _ ->
                let value =
                    """{ "foo": [1, 2] }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithIntList>
                value.foo |> should equal [1; 2]

            it "converts arrays with objects" <| fun _ ->
                let value =
                    """{ "children": [ {"foo": 1}, {"foo": 2} ], "bar": 3}"""
                    |> stringToJson
                    |> jsonToObj<ParentWithList>
                let expected = { children=[{foo= 1};{foo= 2}];bar= 3}
                value |> should equal expected
        ]

        it "should return parent type" <| fun _ ->
            let value =
                """{ "child": { "foo": 42 }, "bar": 43 }"""
                |> stringToJson
                |> jsonToObj<ParentType>
            value.child.foo |> should equal 42
            value.bar |> should equal 43

        describe "type mismatch" [
            describe "when json does not contain correct parameter" [
                it "should return a proper error type" <| fun _ ->
                    """{ "bar": 42 }"""
                    |> stringToJson
                    |> toInstance<FooTypeWithInt>
                    |> getFailure
                    |> should matchRegex "could not find data for record value 'foo'"
            ]
            describe "when json data and record value are incompatible" [
                it "should return a proper error type" <| fun _ ->
                    """{ "foo": "bar" }"""
                    |> stringToJson
                    |> toInstance<FooTypeWithInt>
                    |> getFailure
                    |> should matchRegex "incompatible type"
            ]
        ]

        describe "Target type is a map" [
            it "Should convert object member names to keys in the map" <| fun _ ->
                let actual =
                    """{ "children": {
                         "a" : { "foo": 42 },
                         "b" : { "foo": 43 } } }"""
                    |> stringToJson
                    |> jsonToObj<ParentTypeWithMap>
                actual.children.["a"].foo |> should equal 42
                actual.children.["b"].foo |> should equal 43

            it "should handle when child object is of wrong type" <| fun _ ->
                let actual =
                    """{ "children": {
                         "a" : { "foo": "blah" },
                         "b" : { "foo": 43 } } }"""
                    |> stringToJson
                    |> toInstance<ParentTypeWithMap>
                match actual with
                | Failure _ -> ()
                | Success _ -> failwith "Should be a failure"
        ]
    ]