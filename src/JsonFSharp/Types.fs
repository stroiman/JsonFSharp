namespace JsonFSharp

type JsonValue =
    | JsonString of string
    | JsonNumber of double
    | JsonBool of bool
    | JsonNull
    | JsonArray of JsonValue list

type ParseResult =
    | Success of JsonValue
    | Failure of string
