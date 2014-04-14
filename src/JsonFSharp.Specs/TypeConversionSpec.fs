module TypeConversionSpec
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open JsonParser

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

let getSuccess = function
    | Success(x) -> x
    | Failure(x) -> failwith x

let stringToJson = JsonInput.fromString >> parse >> getSuccess
let jsonToObj<'T> = toInstance<'T> >> getSuccess

let specs = 
    describe "Type conversions" [
        it "should return instance of specified type" <| fun () ->
            let value = 
                """{ "foo": "bar", "foo2": 42 }"""      
                |> stringToJson
                |> jsonToObj<FooType>
            value.foo |> should equal "bar"

        it "should return parent type" <| fun () ->
            let value =
                """{ "child": { "foo": 42 }, "bar": 43 }"""
                |> stringToJson
                |> jsonToObj<ParentType>
            value.child.foo |> should equal 42
            value.bar |> should equal 43
    ]