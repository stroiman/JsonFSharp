module JsonFSharp.JsonParser
open System.IO
open JsonFSharp.Lexer
open JsonFSharp.Parsers
open System.Linq

type JsonInput =
    | StringInput of string
    | StreamInput of Stream
    with 
        static member fromString input = StringInput input
        static member fromStream input = StreamInput input
    
let parse input =
    let lexbuf = 
        match input with
        | StringInput str -> Lexing.LexBuffer<char>.FromString str
        | StreamInput str ->
            let reader = new StreamReader(str)
            Lexing.LexBuffer<char>.FromTextReader reader
    try
        Parsers.start Lexer.token lexbuf
    with
        | e -> Failure (e.ToString())


let toInstance<'T> json =
    let getObjectData json =
        match json with
        | JsonString(x) -> x :> System.Object
        | JsonNumber(x) -> x :> System.Object
        | _ -> failwith "Not implemented"
        
    let rec toInstanceOfType (targetType: System.Type) json =
        match json with
        | JsonObject(obj) ->
            let getValue name = obj.[name]
            let ctor = targetType.GetConstructors().Single()
            let ctorParameters = 
                ctor.GetParameters() 
                |> Array.toList
                |> List.map (fun param -> 
                    let jsonValue = getValue param.Name
                    match jsonValue with
                    | JsonObject(x) -> toInstanceOfType param.ParameterType jsonValue
                    | _ ->
                        let value = jsonValue |> getObjectData
                        System.Convert.ChangeType(value, param.ParameterType))
                |> List.toArray
            ctor.Invoke(ctorParameters)
        | _ -> failwith "Not an object"

    let targetType = typeof<'T>
    Success(toInstanceOfType targetType json :?> 'T)

