module TypeConversionSpec
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
    ]