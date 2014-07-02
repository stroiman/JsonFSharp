module Helpers
open FSpec.Core.Dsl
open FSpec.Core.Matchers
open JsonFSharp
open JsonParser

let parseString = JsonInput.fromString >> parse

let shouldEqualJson expected actual =
    actual |> should equal (Success expected)

let shouldEqualString expected actual =
    actual |> shouldEqualJson (JsonString expected)

let shouldEqualObject expectedProperties actual =
    let expected = JsonObject (expectedProperties |> Map.ofList)
    actual |> shouldEqualJson expected

let spec name input expected =
    it name <| fun _ ->
        input
        |> parseString
        |> shouldEqualJson expected
        
let stringSpec name input expectedOutput =
    spec name input (JsonString expectedOutput)

let numberSpec name input expectedOutput =
    examples [
        context "When using a US locale" [
            before <| fun _ -> System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.CreateSpecificCulture("en-US")
            
            spec name input (JsonNumber expectedOutput)
        ]
        context "When using a da-DK locale" [
            before <| fun _ -> System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.CreateSpecificCulture("da-DK")

            spec name input (JsonNumber expectedOutput)
        ]
    ]
