module ArraySpecs

open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open Helpers

let specs =
    describe "Parsing arrays" [
        it "parses array with single element" <| fun () ->
            "[null]"
            |> JsonParser.parse
            |> shouldEqualJson (JsonArray [JsonNull])
    ]