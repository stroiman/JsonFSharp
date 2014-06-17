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

let toInstance<'T> json =
    let changeType (targetType: System.Type) value = 
        try
            Success(System.Convert.ChangeType(value :> System.Object, targetType))
        with
            | e -> Failure "incompatible types"

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

    let rec toInstanceOfType (targetType: System.Type) json =
        let converter = new Converter()
        let rec coerceToType targetType jsonValue  =
            let toObj value = changeType targetType value
            match jsonValue with
            | JsonObject x -> toInstanceOfType targetType jsonValue 
            | JsonString x -> toObj x
            | JsonNumber x -> toObj x
            | JsonBool x -> toObj x
            | JsonNull -> toObj null
            | JsonArray arr ->
                match targetType with
                | ListType t ->
                    arr |> List.map (fun x -> 
                            match coerceToType t x with
                            | Failure f -> failwith "not supported yet"
                            | Success s -> s)
                    |> converter.changeListToType t 
                    |> Success
                | _ -> Failure "Not a generic list type"

        match json with
        | JsonObject(obj) ->
            match targetType with
            | StringMap t -> 
                obj 
                |> Map.toList 
                |> bindList (fun (x,y) ->
                    match y |> toInstanceOfType t with
                    | Success z -> Success (x,z)
                    | Failure z -> Failure z)
                |> TwoTrack.bind (converter.listToTypedMap t >> Success)
            | _ -> 
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
    let mapType = typeof<Map<string,_>>
    match toInstanceOfType targetType json with
    | Success(x) -> Success(x :?> 'T)
    | Failure(x) -> Failure(x)

