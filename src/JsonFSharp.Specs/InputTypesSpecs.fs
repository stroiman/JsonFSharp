module InputTypesSpecs
open FSpec.Core.DslV2
open FSpec.Core.MatchersV2
open JsonFSharp

let shouldBeSuccess actual =
    match actual with
    | Success _ -> ()
    | Failure _ -> failwith "Expected a success"

let specs =
    describe "Input types" [
        it "handles strings" <| fun () ->
            "null" |> JsonParser.parse |> shouldBeSuccess
            
        it "handles UTF8 encoded streams" <| fun () ->
            let buffer = System.Text.Encoding.UTF8.GetBytes ("null")
            let stream = new System.IO.MemoryStream(buffer)
            stream |> JsonParser.parseStream |> shouldBeSuccess
    ]