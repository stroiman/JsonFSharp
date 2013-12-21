module PrimitiveTypesSpec
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp

let specs =
    describe "Parsing primitive types" [
        describe "string parsing" [
            it "handles simple strings" <| fun () ->
                let actual = "\"dummy\"" |> JsonParser.parse
                let expected = JsonString "dummy"
                actual |> should equal expected
        ]
    ]

