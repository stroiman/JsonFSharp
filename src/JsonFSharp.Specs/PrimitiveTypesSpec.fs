module PrimitiveTypesSpec
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp

let shouldEqualJson expected actual =
    actual |> should equal (Success expected)

let shouldEqualString expected actual =
    actual |> shouldEqualJson (JsonString expected)

let stringSpec name input expectedOutput =
    it name <| fun () ->
        input
        |> JsonParser.parse
        |> shouldEqualString expectedOutput

let numberSpec name input expectedOutput =
    it name <| fun () ->
        input
        |> JsonParser.parse
        |> shouldEqualJson (JsonNumber expectedOutput)

let specs =
    describe "Parsing primitive types" [
        describe "string parsing" [
            stringSpec "handles simple strings" "\"dummy\"" "dummy"
            stringSpec "handles empty strings" "\"\"" ""

            describe "escape characters" [
                stringSpec """parses \" """ "\"\\\"\"" "\""
                stringSpec "parses \\\\" "\"\\\\\"" "\\"
                stringSpec "parses \\/" "\"\\/\"" "/"
                stringSpec "parses \\b" "\"\\b\"" "\b"
                stringSpec "parses \\f" "\"\\f\"" "\f"
                stringSpec "parses \\n" "\"\\n\"" "\n"
                stringSpec "parses \\r" "\"\\r\"" "\r"
                stringSpec "parses \\t" "\"\\t\"" "\t"
                stringSpec "parses unicodes" "\"\\u0061\"" "a"
                stringSpec "parses string containg \\b" "\"dummy1\\bdummy2\"" "dummy1\bdummy2"

                describe "bad unicode values" [
                    it "fails when unicode contains 3 characters" <| fun () ->
                        let result = "\"\\u123\"" |> JsonParser.parse
                        match result with
                        | Success(_) -> failwith "Expected fail"
                        | Failure(_) -> ()
                        
                    stringSpec "parses unicode contains 5 characters" "\"\\u00615\"" "a5"
                ]
            ]
        ]

        describe "number parsing" [
            numberSpec "parse whole number" "123" 123.0
        ]
    ]

