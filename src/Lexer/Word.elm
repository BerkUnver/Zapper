﻿module Lexer.Word exposing (..)

whitespaceChars = [' ', '\n', '\u{0009}']

isFirstWord word chars =
    case chars of 
        [] -> 
            case word of
                [] -> 
                    Just []
                _ -> 
                    Nothing
                    
        head :: tail ->
            case word of
                [] -> 
                    if List.member head whitespaceChars then
                        Just tail
                    else 
                        Nothing
                
                firstWordChar :: restWordChars ->
                    if firstWordChar == head then
                        firstWord restWordChars tail
                    else
                        Nothing

match word chars = 
    case chars of
        [] -> word
getFirstWord chars =
    