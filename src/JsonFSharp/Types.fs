namespace JsonFSharp

type JsonValue =
    | JsonString of string
    | JsonNumber of double
    | JsonBool of bool
    | JsonNull
    | JsonArray of JsonValue list
    | JsonObject of Map<string,JsonValue>

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure
    with 
        static member FromOption (errorMsg : 'TFailure) (option : 'TSuccess option) =
            match option with
            | Some s -> Success s
            | None -> Failure errorMsg

type ParseResult = Result<JsonValue,string>

module TwoTrack =
    let bind switchFunction twoTrackInput = 
        match twoTrackInput with
        | Success s -> switchFunction s
        | Failure f -> Failure f

    let (>>=) twoTrackInput switchFunction = 
        bind switchFunction twoTrackInput 
