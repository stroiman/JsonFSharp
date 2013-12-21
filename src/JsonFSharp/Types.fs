namespace JsonFSharp

type JsonValue =
    | JsonString of string
    | JsonNumber of double
    | JsonBool of bool

type ParseResult =
    | Success of JsonValue
    | Failure of string
