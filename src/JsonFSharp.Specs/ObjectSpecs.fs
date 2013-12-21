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
    ]