module JsonFSharp.JsonParser
open System.IO
open JsonFSharp.Internals
open JsonFSharp.Internals.Lexer
open System.Linq
open TwoTrack
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text.Lexing

type JsonInput =
    | StringInput of string
    | StreamInput of Stream
    with 
        static member fromString input = StringInput input
        static member fromStream input = StreamInput input
    
let parse input =
    let createLexBuffer = function
        | StringInput str -> LexBuffer<char>.FromString str
        | StreamInput str ->
            let reader = new StreamReader(str)
            LexBuffer<char>.FromTextReader reader
    try
        input 
        |> createLexBuffer 
        |> Parsers.start Lexer.token
    with
        | e -> Failure (e.ToString())
