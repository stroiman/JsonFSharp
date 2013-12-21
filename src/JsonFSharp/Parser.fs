// Implementation file for parser generated by fsyacc
module JsonFSharp.Parsers
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "parser.fsy"

open JsonFSharp

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | NULL
  | BOOL of (bool)
  | EXCEPTION of (string)
  | DOUBLE of (double)
  | STRING of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_NULL
    | TOKEN_BOOL
    | TOKEN_EXCEPTION
    | TOKEN_DOUBLE
    | TOKEN_STRING
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | NULL  -> 1 
  | BOOL _ -> 2 
  | EXCEPTION _ -> 3 
  | DOUBLE _ -> 4 
  | STRING _ -> 5 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_NULL 
  | 2 -> TOKEN_BOOL 
  | 3 -> TOKEN_EXCEPTION 
  | 4 -> TOKEN_DOUBLE 
  | 5 -> TOKEN_STRING 
  | 8 -> TOKEN_end_of_input
  | 6 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_start 
    | 3 -> NONTERM_start 
    | 4 -> NONTERM_start 
    | 5 -> NONTERM_start 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 8 
let _fsyacc_tagOfErrorTerminal = 6

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | NULL  -> "NULL" 
  | BOOL _ -> "BOOL" 
  | EXCEPTION _ -> "EXCEPTION" 
  | DOUBLE _ -> "DOUBLE" 
  | STRING _ -> "STRING" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | NULL  -> (null : System.Object) 
  | BOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | EXCEPTION _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | DOUBLE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 2us; 1us; 2us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 4us; 1us; 5us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 18us; 20us; |]
let _fsyacc_action_rows = 11
let _fsyacc_actionTableElements = [|5us; 32768us; 1us; 8us; 2us; 6us; 3us; 10us; 4us; 4us; 5us; 2us; 0us; 49152us; 1us; 32768us; 0us; 3us; 0us; 16385us; 1us; 32768us; 0us; 5us; 0us; 16386us; 1us; 32768us; 0us; 7us; 0us; 16387us; 1us; 32768us; 0us; 9us; 0us; 16388us; 0us; 16389us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 6us; 7us; 9us; 10us; 12us; 13us; 15us; 16us; 18us; 19us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 2us; 2us; 2us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 1us; 1us; 1us; 1us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 16386us; 65535us; 16387us; 65535us; 16388us; 16389us; |]
let _fsyacc_reductions ()  =    [| 
# 101 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  JsonFSharp.ParseResult )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 110 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 16 "parser.fsy"
                                        Success (JsonString _1) 
                   )
# 16 "parser.fsy"
                 :  JsonFSharp.ParseResult ));
# 121 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : double)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 17 "parser.fsy"
                                        Success (JsonNumber _1) 
                   )
# 17 "parser.fsy"
                 :  JsonFSharp.ParseResult ));
# 132 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 18 "parser.fsy"
                                      Success (JsonBool _1) 
                   )
# 18 "parser.fsy"
                 :  JsonFSharp.ParseResult ));
# 143 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 19 "parser.fsy"
                                      Success (JsonNull) 
                   )
# 19 "parser.fsy"
                 :  JsonFSharp.ParseResult ));
# 153 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 20 "parser.fsy"
                                       Failure _1 
                   )
# 20 "parser.fsy"
                 :  JsonFSharp.ParseResult ));
|]
# 165 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 9;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  JsonFSharp.ParseResult  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
