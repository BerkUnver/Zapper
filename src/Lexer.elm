module Lexer exposing (..)

import Lexer.Keyword as Keyword exposing (Keyword)
import More.List as List
import SExpr exposing (SExpr)
import Tokenizer 
    
type WasmLiteral
    = Keyword Keyword
    | U32 Int
    | I32 Int
    | F32 Float
    | String (List Char)
    | Id (List Char)
    -- todo : add reserved identifiers
    

fromTokenizerToken : SExpr.Token Tokenizer.WasmString -> Maybe (SExpr.Token WasmLiteral) 
fromTokenizerToken token =
    case token of 
        SExpr.LPar -> 
            Just SExpr.LPar
        
        SExpr.RPar -> 
            Just SExpr.RPar
        
        SExpr.Literal (Tokenizer.String str) -> 
            Just <| SExpr.Literal <| String str
        
        SExpr.Literal (Tokenizer.Id id) ->
            case id of
                '$' :: var -> 
                    Just <| SExpr.Literal <| Id var
                
                _ ->
                    [ Keyword.fromCharList >> Maybe.map Keyword
                    -- todo : add identification for unsigned integers
                    , String.fromList >> String.toInt >> Maybe.map I32
                    , String.fromList >> String.toFloat >> Maybe.map F32
                    ]
                    |> List.firstJust (\func -> func id)
                    |> Maybe.map SExpr.Literal


fromTokenizerTokens : List (SExpr.Token Tokenizer.WasmString) -> Maybe (List (SExpr.Token WasmLiteral))
fromTokenizerTokens tokens = List.tryAll fromTokenizerToken tokens

lex chars = 
    Tokenizer.tokenize chars
    |> Maybe.andThen fromTokenizerTokens