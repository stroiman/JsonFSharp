module TypeConversionSpec
open System.Text.RegularExpressions
open FSpec.Dsl
open FSpec.Matchers
open JsonFSharp
open JsonParser
open JsonConverter

module Helpers =
  type FooLongTupleType = int * int * int * int * int * int * int * int * int
  type FooTypeWithString = { foo : string; }
  type FooTypeWithInt = { foo : int; }
  type FooTypeWithIntOption = { foo : int option; }
  type FooTypeWithBools = { t: bool; f: bool }
  type FooTypeWithTuple = { foo : int * int }
  type FooTypeWithIntList = { foo: int list }
  type FooTypeWithJsonValue = { foo : JsonValue }
  type FooTypeWithJsonValueOption = { foo : JsonValue option }

  type ParentType = {
      child: FooTypeWithInt;
      bar: int
      }
  type ParentTypeWithMapOfInt = {
          children: Map<string,int>
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

  let beFailure = createSimpleMatcher (function | Failure _ -> true | _ -> false)
open Helpers

let specs = 
    describe "Type conversions" [
        describe "target type is an option type" [
            it "is initialized to 'None' when no value exists in the input" <| fun _ ->
                let value =
                    """{ }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithIntOption>
                value.foo.Should (equal None)
        ]
        describe "primitive type conversions" [
            it "converts simple json types" <| fun _ ->
                JsonNumber 42.0
                |> jsonToObj<int>
                |> should (equal 42)
        ]
        describe "Simple type conversions" [
            it "an initialize string values" <| fun _ ->
                let value = 
                    """{ "foo": "bar" }"""      
                    |> stringToJson
                    |> jsonToObj<FooTypeWithString>
                value.foo.Should (equal "bar")
            it "can initialize integer values" <| fun _ ->
                let value = 
                    """{ "foo": 42 }"""      
                    |> stringToJson
                    |> jsonToObj<FooTypeWithInt>
                value.foo.Should (equal 42)
            it "can initialize boolean values" <| fun _ ->
                let value =
                    """{ "t": true, "f": false }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithBools>
                value.t.Should be.True
                value.f.Should be.False
        ]
        describe "null values" [
            it "Converts null to Option.None for option types" <| fun _ ->
                let value =
                    """{ "foo": null }"""
                    |> stringToJson
                    |> jsonToObj<FooTypeWithIntOption>
                value.foo.Should (equal None)

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
                value.foo.Should (equal [1; 2])

            it "converts arrays with objects" <| fun _ ->
                let value =
                    """{ "children": [ {"foo": 1}, {"foo": 2} ], "bar": 3}"""
                    |> stringToJson
                    |> jsonToObj<ParentWithList>
                let expected = { children=[{foo= 1};{foo= 2}];bar= 3}
                value.Should (equal expected)
        ]

        it "should return parent type" <| fun _ ->
            let value =
                """{ "child": { "foo": 42 }, "bar": 43 }"""
                |> stringToJson
                |> jsonToObj<ParentType>
            value.child.foo.Should (equal 42)
            value.bar.Should (equal 43)

        describe "type mismatch" [
            describe "when json does not contain correct parameter" [
                it "should return a proper error type" <| fun _ ->
                    """{ "bar": 42 }"""
                    |> stringToJson
                    |> toInstance<FooTypeWithInt>
                    |> getFailure
                    |> should (be.string.matching "could not find data for record value 'foo'")
            ]
            describe "when json data and record value are incompatible" [
                it "should return a proper error type" <| fun _ ->
                    """{ "foo": "bar" }"""
                    |> stringToJson
                    |> toInstance<FooTypeWithInt>
                    |> getFailure
                    |> should (be.string.matching "cannot change to type")
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
                actual.children.["a"].foo.Should (equal 42)
                actual.children.["b"].foo.Should (equal 43)

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

            it "should handle when child object is not an object" <| fun _ ->
                let actual =
                    """{ "children": {
                         "a" : 42,
                         "b" : 43 } }"""
                    |> stringToJson
                    |> jsonToObj<ParentTypeWithMapOfInt>
                actual.children.["a"].Should (equal 42)
        ]

        describe "Target type contains a tuple" [
            it "fails when source does not contain an array" <| fun _ ->
                """{ "foo": 42 }"""
                |> stringToJson
                |> toInstance<FooTypeWithTuple>
                |> should beFailure

            it "succeeds when source is an array" <| fun _ ->
                """{ "foo": [42,43] }"""
                |> stringToJson
                |> jsonToObj<FooTypeWithTuple>
                |> (fun x -> x.foo)
                |> should (equal (42,43))
              
            it "fails when source array is not long enough" <| fun _ ->
                """{ "foo": [42] }"""
                |> stringToJson
                |> toInstance<FooTypeWithTuple>
                |> should beFailure

            it "ignores extra data in source array" <| fun _ ->
                """{ "foo": [42,43,44] }"""
                |> stringToJson
                |> jsonToObj<FooTypeWithTuple>
                |> (fun x -> x.foo)
                |> should (equal (42,43))
        ]

        describe "Target is a long tuple" [
            it "Handles long tuples" <| fun _ ->
                // Tuples with more than 8 elements are internally created as 
                // nested tuples. This creates some problems.
                """[1,2,3,4,5,6,7,8,9]"""
                |> stringToJson
                |> jsonToObj<FooLongTupleType>
                |> should (equal (1,2,3,4,5,6,7,8,9))
            it "Handles very long tuples" <| fun _ ->
                // Tuples with more than 8 elements are internally created as 
                // nested tuples. This creates some problems.
                """[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]"""
                |> stringToJson
                |> jsonToObj<int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int>
                |> should (equal (1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0))
        ]

        describe "Target type contains a JsonValue property" [
            it "is initialized with the raw JsonValue type" <| fun _ ->
                """{ "foo": 42 }"""
                |> stringToJson
                |> jsonToObj<FooTypeWithJsonValue>
                |> (fun x -> x.foo)
                |> should (equal (JsonNumber 42.0))
        ]

        describe "Target type contains a JsonValue option property" [
            it "is initialized with 'Some JsonValue' when a value exists" <| fun _ ->
                """{ "foo": 42 }"""
                |> stringToJson
                |> jsonToObj<FooTypeWithJsonValueOption>
                |> (fun x -> x.foo)
                |> should (equal (Some (JsonNumber 42.0)))
            it "is initialized with 'None' when no value exists" <| fun _ ->
                """{ }"""
                |> stringToJson
                |> jsonToObj<FooTypeWithJsonValueOption>
                |> (fun x -> x.foo)
                |> should (equal (None))
        ]
    ]
