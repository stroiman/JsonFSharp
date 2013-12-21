module JsonFSharp.JsonParser
open System.IO
open JsonFSharp.Lexer
open JsonFSharp.Parsers

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
