module PrimitiveTypesSpec
open FSpec.Dsl
open FSpec.Matchers
open JsonFSharp
open Helpers

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
                    it "fails when unicode contains 3 characters" <| fun _ ->
                        let result = "\"\\u123\"" |> parseString
                        match result with
                        | Success(_) -> failwith "Expected fail"
                        | Failure(_) -> ()
                        
                    stringSpec "parses unicode contains 5 characters" "\"\\u00615\"" "a5"
                ]
            ]
        ]

        describe "number parsing" [
            numberSpec "parse whole number" "123" 123.0
            numberSpec "parse whole number" "123.25" 123.25
            numberSpec "parse negative number" "-123.25" -123.25
            numberSpec "parse scientific number" "1.2e2" 120.0
            numberSpec "parse scientific number with uppercase E" "1.2E2" 120.0
        ]

        describe "parse boolean" [
            spec "parse true" "true" (JsonBool true)
            spec "parse false" "false" (JsonBool false)
        ]

        spec "parse null" "null" JsonNull
    ]

