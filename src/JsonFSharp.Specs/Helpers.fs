module Helpers
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
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
    it name <| fun () ->
        input
        |> parseString
        |> shouldEqualJson expected
        
let stringSpec name input expectedOutput =
    spec name input (JsonString expectedOutput)

let numberSpec name input expectedOutput =
    spec name input (JsonNumber expectedOutput)
