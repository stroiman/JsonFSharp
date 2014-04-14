module ObjectSpecs
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open Helpers

let specs =
    describe "Object parsing" [
        it "parses simple object" <| fun () ->
            "{}"
            |> parseString
            |> shouldEqualJson (JsonObject Map.empty)

        it "parses object with one property" <| fun () ->
            let expected = [("key", JsonString("value"))]
            """{"key":"value"}"""
            |> parseString
            |> shouldEqualObject expected

        it "parses object with two properties" <| fun () ->
            let expected = [
                ("a", JsonString("value a"))
                ("b", JsonString("value b"))]
            """{"a":"value a","b":"value b"}"""
            |> parseString
            |> shouldEqualObject expected

        it "ignores white space" <| fun () ->
            let expected = [
                ("a", JsonString("value a"))
                ("b", JsonString("value b"))]
            """{ "a" : "value a",
                 "b" : "value b"
               }"""
            |> parseString
            |> shouldEqualObject expected

        it "parses nested objects" <| fun () ->
            let expected = [
                ("a", JsonObject(["b", JsonNumber(42.0)] |> Map.ofList))]
            """{ "a": { "b": 42 } }"""
            |> parseString
            |> shouldEqualObject expected

    ]