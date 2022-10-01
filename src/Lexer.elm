module Lexer exposing (..)

import More.List as List
import NumType exposing (NumType)
import SExpr exposing (SExpr)
import Tokenizer 
    
    
type Token
    = UInt Int
    | Int Int
    | Float Float
    | String String
    | Var String
    
    | Module
    | Func
    | Param 
    | Result
    -- todo : Export, import, type keywords
    | NumType NumType
    | Instr String
    -- todo : add reserved identifiers
    

toString literal = 
    case literal of
        UInt i -> String.fromInt i
        Int i -> String.fromInt i
        Float f -> String.fromFloat f
        String chars -> "\"" ++ chars ++ "\""
        Var chars -> "$" ++ chars
        Module -> "module"
        Func -> "func"
        Param -> "param"
        Result -> "result"
        NumType t -> NumType.toString t
        Instr chars -> chars


fromTokenizerToken : SExpr.Token Tokenizer.WasmString -> SExpr.Token Token
fromTokenizerToken token =
    case token of 
        SExpr.LPar ->
            SExpr.LPar
        
        SExpr.RPar -> 
            SExpr.RPar
        
        SExpr.Literal (Tokenizer.String str) -> 
            SExpr.Literal <| String <| String.fromList str
        
        SExpr.Literal (Tokenizer.Id id) ->
            case id of
                '$' :: var -> 
                    -- todo : Identify invalid characters in variable names
                    SExpr.Literal <| Var <| String.fromList var
                
                _ ->
                    let 
                        str = String.fromList id
                        literal = 
                            case str of
                                "module" -> Module
                                "func" -> Func
                                "param" -> Param
                                "result" -> Result
                                _ ->
                                    [ NumType.fromString >> Maybe.map NumType
                                    , String.toInt >> Maybe.map Int
                                    , String.toFloat >> Maybe.map Float 
                                    ]
                                    |> List.firstJust (\f -> f str)
                                    |> Maybe.withDefault (Instr <| String.fromList id)
                    in
                    SExpr.Literal literal


fromTokenizerTokens : List (SExpr.Token Tokenizer.WasmString) -> List (SExpr.Token Token)
fromTokenizerTokens tokens = List.map fromTokenizerToken tokens


lex chars = 
    Tokenizer.tokenize chars
    |> Maybe.map fromTokenizerTokens