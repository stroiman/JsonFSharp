module JsonFSharp.JsonParser
open JsonFSharp.Lexer
open JsonFSharp.Parsers

let parse input =
    let lexbuf = Lexing.LexBuffer<char>.FromString input
    Parsers.start Lexer.token lexbuf