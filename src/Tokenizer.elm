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


tokenizeString : List Char -> Result String (List Char, List Char)
tokenizeString chars =
    let 
        appendStr char tail = 
            tokenizeString tail
            |> Result.map (\(str, next) -> (char :: str, next))
    in
    case chars of
        [] -> 
            Err "Unclosed string."
        '"' :: tail -> 
            Ok ([], tail)
        '\\' :: tail -> 
            case tail of
                [] -> 
                    Err "Unclosed string ending with illegal escape sequence."
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
                        _ -> Err "Illegal escape sequence in string."
        char :: tail ->
            appendStr char tail      


tokenize : List Char -> Result String (List Token)
tokenize chars = 
    let concatToken rest token = tokenize rest |> Result.map (\tokens -> token :: tokens) in
    case chars of 
        [] -> Ok []
        
        '(' :: tail ->
            concatToken tail LPar
        
        ')' :: tail  -> 
            concatToken tail RPar
        
        '"' :: tail ->
            -- todo : add error message for strings that end and don't have whitespace/parenthesis after
            tokenizeString tail
            |> Result.andThen (\(str, rest) -> String str |> concatToken rest)
        
        ';' :: ';' :: tail ->
            let 
                discardComment str = 
                    case str of 
                        [] -> []
                        '\n' :: rest -> rest
                        _ :: rest -> discardComment rest
            in
            discardComment tail
            |> tokenize
                
        head :: tail ->
            -- todo : Check for non-7-bit ASCII characters and give error
            -- todo : Check for line comments
            -- todo : Check for block comments
            if isWhitespace head then
                tokenize tail
            else
                let 
                    (datum, rest) =  List.splitFirstTrue (\c -> c == '(' || c == ')' || isWhitespace c) tail -- this is weird, can be made more concise.
                in
                head :: datum
                |> Id
                |> concatToken rest
    