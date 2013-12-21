module PrimitiveTypesSpec
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp

let shouldEqualJson expected actual =
    actual |> should equal (Success expected)

let shouldEqualString expected actual =
    actual |> shouldEqualJson (JsonString expected)

let specs =
    describe "Parsing primitive types" [
        describe "string parsing" [
            it "handles simple strings" <| fun () ->
                "\"dummy\"" 
                |> JsonParser.parse
                |> shouldEqualString "dummy"

            describe "escape characters" [
                it "parses \\b" <| fun () ->
                    "\"\\b\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "\b"

                it "parses string containg \\b" <| fun () ->
                    "\"dummy\\bdummy\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "dummy\bdummy"
            ]
        ]
    ]

