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

    
lex : List Tokenizer.Token -> Maybe Token
lex tokens = 
    case tokens of 
        [] -> 
            Nothing
        Tokenizer.LPar :: tail ->
            let 
                parseTail tailTokens = 
                    case tailTokens of
                        [] ->
                            Nothing
                            
                        Tokenizer.RPar :: restTokens ->
                            Just ([], restTokens)
                        
                        Tokenizer.Id id :: restTokens -> 
                            parseTail restTokens
                            |> Maybe.map (\(listSExpr, rest) -> (parseId id :: listSExpr, rest))
                        
                        Tokenizer.String str :: restTokens ->
                            parseTail restTokens
                            |> Maybe.map (\(listSExpr, rest) -> (String (String.fromList str) :: listSExpr, rest))
                        
                        Tokenizer.LPar :: restTokens ->
                            parseTail restTokens
                            |> Maybe.andThen (\(headListSExpr, nextTokens) -> 
                                parseTail nextTokens 
                                 |> Maybe.map (\(listSExpr, leftoverTokens) -> 
                                    (Scope headListSExpr :: listSExpr, leftoverTokens)))
            in
            case parseTail tail of
                Just (listSExpr, []) -> Just <| Scope listSExpr
                _ -> Nothing
        
        Tokenizer.Id id :: [] -> 
            Just <| parseId id
            
        Tokenizer.String str :: [] ->
            Just <| String <| String.fromList str
        
        _ -> 
            Nothing
