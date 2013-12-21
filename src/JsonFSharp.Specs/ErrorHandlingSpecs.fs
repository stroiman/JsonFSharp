module ErrorHandlingSpecs
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp

let specs =
    describe "Error handling" [
        it "handles invalid input" <| fun () ->
            let actual = "invalid" |> JsonParser.parse
            match actual with
            | Success _ -> failwith "Expected fail"
            | Failure _ -> ()
    ]

