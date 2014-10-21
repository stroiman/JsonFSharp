module JsonFSharp.JsonParser
open System.IO
open JsonFSharp.Lexer
open JsonFSharp.Parsers
open System.Linq
open TwoTrack
open Microsoft.FSharp.Reflection

type JsonInput =
    | StringInput of string
    | StreamInput of Stream
    with 
        static member fromString input = StringInput input
        static member fromStream input = StreamInput input
    
let parse input =
    let createLexBuffer = function
        | StringInput str -> Lexing.LexBuffer<char>.FromString str
        | StreamInput str ->
            let reader = new StreamReader(str)
            Lexing.LexBuffer<char>.FromTextReader reader
    try
        input 
        |> createLexBuffer 
        |> Parsers.start Lexer.token
    with
        | e -> Failure (e.ToString())

type Converter() =
    member self.changeListType<'T> (input : System.Object list) =
        input |> List.map (fun x -> x :?> 'T)

    member self.changeListToType (targetType : System.Type) (input : System.Object list ) =
        typeof<Converter>.GetMethod("changeListType").MakeGenericMethod(targetType).Invoke(self, [|input|])
        |> Success

    member self.toMap<'T> (l:(string*obj) list) =
        let rec work (l:(string*obj) list) map =
            match l with
            | [] -> map
            | (k,v)::xs -> 
                map 
                |> Map.add k (v :?> 'T)
                |> work xs
        Map.empty<string,'T> |> work l
        
    member self.listToTypedMap (targetType: System.Type) (l:(string*obj) list) =
        typeof<Converter>.GetMethod("toMap").MakeGenericMethod(targetType).Invoke(self, [|l|])
        |> Success

let toInstance<'T> json =
    let changeType (targetType: System.Type) value = 
        try
            Success(System.Convert.ChangeType(value :> System.Object, targetType))
        with
            | e -> Failure (sprintf "cannot change to type %A - json: %A" targetType.Name value)

    let (|StringMap|_|) (targetType: System.Type) =
        let mapType = typedefof<Map<_,_>>
        if not targetType.IsGenericType then
            None
        else if targetType.GetGenericTypeDefinition () = mapType then
            let stringType = typeof<string>
            match targetType.GetGenericArguments () with
            | [|stringType;x|] -> Some(x)
            | _ -> None
        else
            None

    let (|RecordType|_|) (t : System.Type) =
        if (Microsoft.FSharp.Reflection.FSharpType.IsRecord t) then
            Some t
        else None

    let (|ListType|_|) (t : System.Type) =
        match t with
        | x when not(x.IsGenericType) -> None
        | x when not(x.GetGenericTypeDefinition().Name.StartsWith("FSharpList")) -> None
        | x -> Some (x.GetGenericArguments().Single())

    let (|TupleType|_|) (t : System.Type) =
        if (Microsoft.FSharp.Reflection.FSharpType.IsTuple t) then
            Some t
        else None

    let invokeCtor (targetType : System.Type) args =
        let ctor = targetType.GetConstructors().Single()
        args |> List.toArray |> ctor.Invoke |> Success

    let rec pairWith source target =
        match (source,target) with
        | (x::xs,y::ys) -> 
            (pairWith xs ys)
            |> bind (fun z -> Success ((x,y)::z))
        | (_,[]) -> Success []
        | ([],_) -> Failure "Not enough elements"

    let converter = new Converter()
    let rec coerceToType (targetType: System.Type) (json:JsonValue) =
        let toObj value = changeType targetType value
        match targetType with
        | x when x = typeof<JsonValue> -> json :> obj |> Success
        | x when x = typeof<Option<JsonValue>> -> 
            let result = 
              match json with
              | JsonNull -> None 
              | _ -> (Some json) 
            result :> obj |> Success
        | ListType t ->
            match json with
            | JsonArray arr ->
                arr |> List.map (fun x -> 
                        match coerceToType t x with
                        | Failure f -> failwith "not supported yet"
                        | Success s -> s)
                |> converter.changeListToType t 
            | _ -> Failure "Expected a list"
        | StringMap t -> 
            match json with
            | JsonObject obj ->
                obj 
                |> Map.toList 
                |> bindList (fun (x,y) ->
                    match y |> coerceToType t with
                    | Success z -> Success (x,z)
                    | Failure z -> Failure z)
                >>= converter.listToTypedMap t
            | _ -> Failure "Expected an object for a string map"
        | RecordType t -> 
            match json with
            | JsonObject(obj) ->
                let getConstructorArgument (arg: System.Reflection.ParameterInfo) =
                    let argType = arg.ParameterType
                    let optionalArg = argType.IsGenericType && (argType.GetGenericTypeDefinition().Name.StartsWith "FSharpOption")
                    arg.Name
                    |> obj.TryFind
                    |> fun x ->
                        match x, optionalArg with
                        | Some x, _ -> x |> Success
                        | None, true -> JsonNull |> Success
                        | None, false -> Failure (sprintf "could not find data for record value '%s'" arg.Name)
                    >>= coerceToType arg.ParameterType
                let ctor = targetType.GetConstructors().Single()
                ctor.GetParameters() 
                |> Array.toList
                |> bindList getConstructorArgument
                >>= invokeCtor targetType
            | _ -> Failure "Json object expected"
        | TupleType t ->
            match json with
            | JsonArray(arr) ->
                FSharpType.GetTupleElements t
                |> Array.toList
                |> pairWith arr
                >>= bindList (fun (x,targetType) -> coerceToType targetType x)
                >>= (invokeCtor targetType)
            | _ -> Failure "Json array expected for tuple type"
        | _ -> match json with
               | JsonString x -> toObj x
               | JsonNumber x -> toObj x
               | JsonBool x -> toObj x
               | JsonNull -> toObj null
               | _ -> Failure "Unexpected type"

    let targetType = typeof<'T>
    let mapType = typeof<Map<string,_>>
    match coerceToType targetType json with
    | Success(x) -> Success(x :?> 'T)
    | Failure(x) -> Failure(x)
