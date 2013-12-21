module ErrorHandlingSpecs
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp
open Helpers

let specs =
    describe "Error handling" [
        it "handles invalid input" <| fun () ->
            let actual = "invalid" |> parseString
            match actual with
            | Success _ -> failwith "Expected fail"
            | Failure _ -> ()

        it "handles invalid input in array" <| fun () ->
            let actual = "[invalid]" |> parseString
            match actual with
            | Success _ -> failwith "Expected fail"
            | Failure _ -> ()
    ]
