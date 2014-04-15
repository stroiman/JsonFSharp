module JsonFSharp.JsonParser
open System.IO
open JsonFSharp.Lexer
open JsonFSharp.Parsers
open System.Linq
open TwoTrack

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
    let changeType (targetType: System.Type) value = 
        try
            Success(System.Convert.ChangeType(value :> System.Object, targetType))
        with
            | e -> Failure "incompatible types"

    let rec toInstanceOfType (targetType: System.Type) json =
        let coerceToType targetType jsonValue  =
            let toObj value = changeType targetType value
            match jsonValue with
            | JsonObject(x) -> toInstanceOfType targetType jsonValue 
            | JsonString(x) -> toObj x
            | JsonNumber(x) -> toObj x
            | _ -> failwith "Not implemented"

        match json with
        | JsonObject(obj) ->
            let getValue name = 
                name 
                |> obj.TryFind
                |> Result.FromOption (sprintf "could not find data for record value '%s'" name)
            
            let getConstructorArgument (arg: System.Reflection.ParameterInfo) =
                getValue arg.Name
                >>= coerceToType arg.ParameterType

            let rec getConstructorParameters (args: System.Reflection.ParameterInfo list) =
                match args with
                | [] -> Success([])
                | head::tail ->
                    getConstructorArgument head 
                    |> bind (fun head -> 
                        getConstructorParameters tail 
                        |> bind (fun tail -> Success(head::tail)))

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

