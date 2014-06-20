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
    let rec coerceToType (targetType: System.Type) json =
        if (targetType = typeof<JsonValue>) then 
            json :> obj |> Success
        else
            let toObj value = changeType targetType value
            match json with
            | JsonString x -> toObj x
            | JsonNumber x -> toObj x
            | JsonBool x -> toObj x
            | JsonNull -> toObj null
            | JsonArray arr ->
                match targetType with
                | TupleType t ->
                    let folder state targetType = 
                        match state with
                        | Failure x -> Failure x
                        | Success (x,y) ->
                            match y with
                            | [] -> Failure "Not enough array elements"
                            | y::z -> 
                                coerceToType targetType y 
                                >>= (fun b -> Success (b::x,z))
                    FSharpType.GetTupleElements t
                    |> Array.toList
                    |> pairWith arr
                    >>= bindList (fun (x,targetType) -> coerceToType targetType x)
                    >>= (invokeCtor targetType)

                | ListType t ->
                    arr |> List.map (fun x -> 
                            match coerceToType t x with
                            | Failure f -> failwith "not supported yet"
                            | Success s -> s)
                    |> converter.changeListToType t 
                | _ -> Failure "Not a generic list type"

            | JsonObject(obj) ->
                match targetType with
                | StringMap t -> 
                    obj 
                    |> Map.toList 
                    |> bindList (fun (x,y) ->
                        match y |> coerceToType t with
                        | Success z -> Success (x,z)
                        | Failure z -> Failure z)
                    >>= converter.listToTypedMap t
                | _ -> 
                    let getConstructorArgument (arg: System.Reflection.ParameterInfo) =
                        arg.Name
                        |> obj.TryFind
                        |> Result.FromOption (sprintf "could not find data for record value '%s'" arg.Name)
                        >>= coerceToType arg.ParameterType

                    let ctor = targetType.GetConstructors().Single()
                    ctor.GetParameters() 
                    |> Array.toList
                    |> bindList getConstructorArgument
                    >>= invokeCtor targetType

    let targetType = typeof<'T>
    let mapType = typeof<Map<string,_>>
    match coerceToType targetType json with
    | Success(x) -> Success(x :?> 'T)
    | Failure(x) -> Failure(x)
