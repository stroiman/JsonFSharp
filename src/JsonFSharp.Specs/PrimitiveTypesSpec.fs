module PrimitiveTypesSpec
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp

let specs =
    describe "Parsing primitive types" [
        describe "string parsing" [
            it "handles simple strings" <| fun () ->
                let actual = "\"dummy\"" |> JsonParser.parse
                let expected = Success (JsonString "dummy")
                actual |> should equal expected

            describe "escape characters" [
                it "parses \\b" <| fun () ->
                    let actual = "\"\\b\"" |> JsonParser.parse
                    let expected = Success (JsonString "\b")
                    actual |> should equal expected
            ]
        ]
    ]

