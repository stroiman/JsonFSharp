module ObjectSpecs
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open Helpers

let specs =
    describe "Object parsing" [
        it "parses simple object" <| fun () ->
            "{}"
            |> JsonParser.parse
            |> shouldEqualJson (JsonObject Map.empty)

        it "parses object with one property" <| fun () ->
            let expected = [("key", JsonString("value"))]
            """{"key":"value"}"""
            |> JsonParser.parse
            |> shouldEqualObejct expected
    ]