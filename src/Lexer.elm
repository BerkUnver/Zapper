module Lexer exposing (..)

import Lexer.Keyword as Keyword exposing (Keyword)
import MoreList
import Tokenizer
    
type Token 
    = Keyword Keyword
    | U32 Int
    | I32 Int
    | F32 Float
    | String (List Char)
    | Id (List Char)
    | LPar
    | RPar
    -- todo : add reserved identifiers
    

fromTokenizerToken token =
    case token of 
        Tokenizer.LPar -> Just LPar
        Tokenizer.RPar -> Just RPar
        Tokenizer.StringLiteral str -> Just <| String str
        Tokenizer.Id id ->
            case id of
                '$' :: var -> Just <| Id var
                _ ->
                    [ Keyword.fromCharList >> Maybe.map Keyword
                    -- todo : add identification for unsigned integers
                    , String.fromList >> String.toInt >> Maybe.map I32
                    , String.fromList >> String.toFloat >> Maybe.map F32
                    ]
                    |> MoreList.firstJust (\func -> func id)


fromTokenizerTokens : List Tokenizer.Token -> Maybe (List Token)
fromTokenizerTokens tokens =
    case tokens of
        [] -> Just []
        head :: tail ->
            fromTokenizerTokens tail
            |> Maybe.andThen (\newTokens -> fromTokenizerToken head |> Maybe.map (\newHead -> newHead :: newTokens))

lex chars = 
    Tokenizer.tokenize chars
    |> Maybe.andThen fromTokenizerTokens