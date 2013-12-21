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
                it """parses \" """ <| fun () ->
                    "\"\\\"\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "\""

                it "parses \\\\" <| fun () ->
                    "\"\\\\\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "\\"

                it "parses \\/" <| fun () ->
                    "\"\\/\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "/"

                it "parses \\b" <| fun () ->
                    "\"\\b\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "\b"

                it "parses \\f" <| fun () ->
                    "\"\\f\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "\f"

                it "parses \\n" <| fun () ->
                    "\"\\n\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "\n"

                it "parses \\r" <| fun () ->
                    "\"\\r\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "\r"

                it "parses \\t" <| fun () ->
                    "\"\\t\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "\t"

                it "parses string containg \\b" <| fun () ->
                    "\"dummy\\bdummy\"" 
                    |> JsonParser.parse
                    |> shouldEqualString "dummy\bdummy"
            ]
        ]
    ]

