module Helpers
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp

let shouldEqualJson expected actual =
    actual |> should equal (Success expected)

let shouldEqualString expected actual =
    actual |> shouldEqualJson (JsonString expected)

let spec name input expected =
    it name <| fun () ->
        input
        |> JsonParser.parse
        |> shouldEqualJson expected
        
let stringSpec name input expectedOutput =
    spec name input (JsonString expectedOutput)

let numberSpec name input expectedOutput =
    spec name input (JsonNumber expectedOutput)
