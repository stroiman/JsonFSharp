module ArraySpecs
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open Helpers

let specs =
    describe "Parsing arrays" [
        it "parses empty array" <| fun () ->
            "[]"
            |> parseString
            |> shouldEqualJson (JsonArray [])

        it "parses array with single element" <| fun () ->
            "[null]"
            |> parseString
            |> shouldEqualJson (JsonArray [JsonNull])

        it "parses array with two elements" <| fun () ->
            let expected = JsonArray [
                               JsonString "one"
                               JsonString "two" ]
            "[\"one\",\"two\"]"
            |> parseString
            |> shouldEqualJson expected
    ]