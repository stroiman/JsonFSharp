module ObjectSpecs
open FSpec.Core.Dsl
open FSpec.Core.Matchers
open JsonFSharp
open Helpers

let specs =
    describe "Object parsing" [
        it "parses simple object" <| fun _ ->
            "{}"
            |> parseString
            |> shouldEqualJson (JsonObject Map.empty)

        it "parses object with one property" <| fun _ ->
            let expected = [("key", JsonString("value"))]
            """{"key":"value"}"""
            |> parseString
            |> shouldEqualObject expected

        it "parses object with two properties" <| fun _ ->
            let expected = [
                ("a", JsonString("value a"))
                ("b", JsonString("value b"))]
            """{"a":"value a","b":"value b"}"""
            |> parseString
            |> shouldEqualObject expected

        it "ignores white space" <| fun _ ->
            let expected = [
                ("a", JsonString("value a"))
                ("b", JsonString("value b"))]
            """{ "a" : "value a",
                 "b" : "value b"
               }"""
            |> parseString
            |> shouldEqualObject expected

        it "parses nested objects" <| fun _ ->
            let expected = [
                ("a", JsonObject(["b", JsonNumber(42.0)] |> Map.ofList))]
            """{ "a": { "b": 42 } }"""
            |> parseString
            |> shouldEqualObject expected

    ]