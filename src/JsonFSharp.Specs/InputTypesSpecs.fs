module InputTypesSpecs
open FSpec.Dsl
open FSpec.Matchers
open JsonFSharp
open JsonParser

let shouldBeSuccess actual =
    match actual with
    | Success _ -> ()
    | Failure _ -> failwith "Expected a success"

let specs =
    describe "Input types" [
        it "handles strings" <| fun _ ->
            "null" 
            |> JsonInput.fromString
            |> JsonParser.parse 
            |> shouldBeSuccess
            
        it "handles UTF8 encoded streams" <| fun _ ->
            let buffer = System.Text.Encoding.UTF8.GetBytes ("null")
            let stream = new System.IO.MemoryStream(buffer)
            stream 
            |> JsonInput.fromStream
            |> JsonParser.parse
            |> shouldBeSuccess
    ]