module Lexer exposing (..)

import More.List as List
import NumType exposing (NumType)
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
    | Local
    -- todo : Export, import, type keywords
    | NumType NumType
    | Instr String
    -- todo : add reserved identifiers
    
    | Scope (List Token)
    

parseId : List Char -> Token
parseId id = 
    case id of
        '$' :: var -> 
            -- todo : Identify invalid characters in variable names
            Var <| String.fromList var
        
        keyword ->
            case String.fromList keyword of
                "module" -> Module
                "func" -> Func
                "param" -> Param
                "result" -> Result
                "local" -> Local
                str ->

                    [ NumType.fromString >> Maybe.map NumType
                    , String.toInt >> Maybe.map Int
                    , String.toFloat >> Maybe.map Float 
                    ]
                    |> List.firstJust (\f -> f str)
                    |> Maybe.withDefault (Instr str)         

    
lex : List Tokenizer.Token -> Result String Token
lex tokens =
    case tokens of 
        [] -> 
            Err "Empty list of tokens."
        Tokenizer.LPar :: tail ->
            let 
                parseTail : List Tokenizer.Token -> Result String (List Token, List Tokenizer.Token)
                parseTail tailTokens = 
                    case tailTokens of
                        [] ->
                            Err "Unclosed parenthesis."
                            
                        Tokenizer.RPar :: restTokens ->
                            Ok ([], restTokens)
                        
                        Tokenizer.Id id :: restTokens -> 
                            parseTail restTokens
                            |> Result.map (\(listSExpr, rest) -> (parseId id :: listSExpr, rest))
                        
                        Tokenizer.String str :: restTokens ->
                            parseTail restTokens
                            |> Result.map (\(listSExpr, rest) -> (String (String.fromList str) :: listSExpr, rest))
                        
                        Tokenizer.LPar :: restTokens ->
                            parseTail restTokens
                            |> Result.andThen (\(headListSExpr, nextTokens) -> 
                                parseTail nextTokens 
                                 |> Result.map (\(listSExpr, leftoverTokens) -> 
                                    (Scope headListSExpr :: listSExpr, leftoverTokens)))
            in
            case parseTail tail of
                Ok (listSExpr, []) -> Ok <| Scope listSExpr
                Ok (_, _) -> Err "Trailing tokens after last parenthesis"
                Err e -> Err e
        
        Tokenizer.Id id :: [] -> 
            Ok <| parseId id
            
        Tokenizer.String str :: [] ->
            Ok <| String <| String.fromList str
        
        _ -> 
            Err "Illegal sequence of tokens."
