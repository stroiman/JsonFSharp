module ArraySpecs
open FSpec.Core.Dsl
open FSpec.Core.Matchers
open JsonFSharp
open Helpers

let specs =
    describe "Parsing arrays" [
        it "parses empty array" <| fun _ ->
            "[]"
            |> parseString
            |> shouldEqualJson (JsonArray [])

        it "parses array with single element" <| fun _ ->
            "[null]"
            |> parseString
            |> shouldEqualJson (JsonArray [JsonNull])

        it "parses array with two elements" <| fun _ ->
            let expected = JsonArray [
                               JsonString "one"
                               JsonString "two" ]
            "[\"one\",\"two\"]"
            |> parseString
            |> shouldEqualJson expected
    ]