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


type Converter() =
    member self.changeListType<'T> (input : System.Object list) =
        input |> List.map (fun x -> x :?> 'T)

    member self.changeListToType (targetType : System.Type) (input : System.Object list ) =
        typeof<Converter>.GetMethod("changeListType").MakeGenericMethod(targetType).Invoke(self, [|input|])
    

let toInstance<'T> json =
    let changeType (targetType: System.Type) value = 
        try
            Success(System.Convert.ChangeType(value :> System.Object, targetType))
        with
            | e -> Failure "incompatible types"

    let rec toInstanceOfType (targetType: System.Type) json =
        let rec coerceToType targetType jsonValue  =
            let toObj value = changeType targetType value
            match jsonValue with
            | JsonObject(x) -> toInstanceOfType targetType jsonValue 
            | JsonString(x) -> toObj x
            | JsonNumber(x) -> toObj x
            | JsonBool(x) -> toObj x
            | JsonNull -> toObj null
            | JsonArray(x) ->
                if not(targetType.IsGenericType) then failwith "Not a generic type"
                if not(targetType.GetGenericTypeDefinition().Name.StartsWith("FSharpList")) then failwith "Not a list type"
                let listType = targetType.GetGenericArguments().Single()
                let temp = 
                    x 
                    |> List.map (fun x -> 
                        match coerceToType listType x with
                        | Failure f -> failwith "not supported yet"
                        | Success s -> s)
                let converter = new Converter()
                let result = converter.changeListToType listType temp
                Success(result)

        match json with
        | JsonObject(obj) ->
            let getValue name = 
                name 
                |> obj.TryFind
                |> Result.FromOption (sprintf "could not find data for record value '%s'" name)
            
            let getConstructorArgument (arg: System.Reflection.ParameterInfo) =
                getValue arg.Name
                >>= coerceToType arg.ParameterType

            let ctor = targetType.GetConstructors().Single()
            ctor.GetParameters() 
            |> Array.toList
            |> bindList getConstructorArgument
            >>= fun x -> Success(ctor.Invoke(x |> List.toArray))
        | _ -> Failure("Not an object")

    let targetType = typeof<'T>
    match toInstanceOfType targetType json with
    | Success(x) -> Success(x :?> 'T)
    | Failure(x) -> Failure(x)

