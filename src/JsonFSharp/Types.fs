namespace JsonFSharp

type JsonValue =
    | JsonString of string

type ParseResult =
    | Success of JsonValue
    | Failure of string
