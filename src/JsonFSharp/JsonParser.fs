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
            let getValue = obj.TryFind
            let getConstructorArgument (arg: System.Reflection.ParameterInfo) =
                let jsonValueOption = getValue arg.Name
                match jsonValueOption with
                | None -> Failure(sprintf "could not find data for record value '%s'" arg.Name)
                | Some(jsonValue) ->
                    match jsonValue with
                    | JsonObject(x) ->
                        match toInstanceOfType arg.ParameterType jsonValue with
                        | Success(x) -> Success(x)
                        | Failure(x) -> Failure(x)
                    | _ ->
                        let value = jsonValue |> getObjectData
                        Success(System.Convert.ChangeType(value, arg.ParameterType))

            let rec getConstructorParameters (args: System.Reflection.ParameterInfo list) =
                match args with
                | [] -> Success([])
                | head::tail ->
                    match getConstructorParameters tail with
                    | Failure(x) -> Failure(x)
                    | Success(x) -> 
                        match getConstructorArgument head with
                        | Success(y) -> Success(y :: x)
                        | Failure(y) -> Failure(y)

            let ctor = targetType.GetConstructors().Single()
            let ctorParameters = 
                ctor.GetParameters() 
                |> Array.toList
                |> getConstructorParameters
            match ctorParameters with
            | Success(x) -> Success(ctor.Invoke(x |> List.toArray))
            | Failure(x) -> Failure(x)
        | _ -> Failure("Not an object")

    let targetType = typeof<'T>
    match toInstanceOfType targetType json with
    | Success(x) -> Success(x :?> 'T)
    | Failure(x) -> Failure(x)

