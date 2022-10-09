module Tokenizer exposing (..)

import More.List as List

type Token
    = String (List Char)
    | Id (List Char)
    | LPar
    | RPar
    

whitespaceChars = [' ', '\n', '\r', '\u{0009}']
isWhitespace char =
    whitespaceChars
    |> List.member char    


tokenizeString : List Char -> Maybe (List Char, List Char)
tokenizeString chars =
    let 
        appendStr char tail = 
            tokenizeString tail
            |> Maybe.map (\(str, next) -> (char :: str, next))
    in
    case chars of
        [] -> 
            Nothing
        '"' :: tail -> 
            Just ([], tail)
        '\\' :: tail -> 
            case tail of
                [] -> 
                    Nothing
                specifier :: rest ->
                    let mapRest char = appendStr char rest in
                    case specifier of
                        't' -> mapRest '\t'
                        'n' -> mapRest '\n'
                        'r' -> mapRest '\r'
                        '"' -> mapRest '"'
                        '\''  -> mapRest '\''
                        '\\' -> mapRest '\\'
                        -- todo : implement unicode format specifiers and hex literals in strings
                        _ -> Nothing
        char :: tail ->
            appendStr char tail


tokenize : List Char -> Maybe (List Token)
tokenize chars = 
    case chars of
        [] ->
            Just []
            
        head :: tail ->
            let concatToken rest token = tokenize rest |> Maybe.map (\tokens -> token :: tokens) in
            case head of
                '(' -> 
                    concatToken tail LPar
                ')' ->
                    concatToken tail RPar
                '"' -> 
                    -- todo : add error message for strings that end and don't have whitespace/parenthesis after
                    tokenizeString tail
                    |> Maybe.andThen (\(str, rest) -> 
                        String str
                        |> concatToken rest)
                _ ->
                    -- todo : Check for non-7-bit ASCII characters and give error
                    -- todo : Check for line comments
                    -- todo : Check for block comments
                    if isWhitespace head then
                        tokenize tail
                    else
                        let 
                            (datum, rest) = 
                                List.splitFirstTrue (\c -> c == '(' || c == ')' || isWhitespace c ) tail
                        in
                        head :: datum
                        |> Id
                        |> concatToken rest