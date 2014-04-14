namespace JsonFSharp

type JsonValue =
    | JsonString of string
    | JsonNumber of double
    | JsonBool of bool
    | JsonNull
    | JsonArray of JsonValue list
    | JsonObject of Map<string,JsonValue>

type Result<'TSuccess> =
    | Success of 'TSuccess
    | Failure of string

type ParseResult = Result<JsonValue>