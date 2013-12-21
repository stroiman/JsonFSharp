module JsonFSharp.JsonParser
open JsonFSharp.Lexer
open JsonFSharp.Parsers

let parse input =
    let lexbuf = Lexing.LexBuffer<char>.FromString input
    try
        Parsers.start Lexer.token lexbuf
    with
        | e -> Failure (e.ToString())

let parseStream (input : System.IO.Stream) =
    use reader = new System.IO.StreamReader(input)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try
        Parsers.start Lexer.token lexbuf
    with
        | e -> Failure (e.ToString())