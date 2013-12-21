namespace JsonFSharp

type JsonValue =
    | JsonString of string
    | JsonNumber of double

type ParseResult =
    | Success of JsonValue
    | Failure of string
