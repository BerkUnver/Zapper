module Lexer exposing (..)

import MoreList

type Token 
    = LPar
    | RPar
    | StringLiteral (List Char)
    | Id (List Char)


toListChar token = 
    case token of
        LPar -> ['(']
        RPar -> [')']
        StringLiteral s -> s
        Id id -> id


whitespaceChars = [' ', '\n', '\u{0009}']

isWhitespace char =
    whitespaceChars
    |> List.member char    

tokenizeString chars = -- todo : more descriptive errors
    let 
        appendStr char tail = 
            tokenizeString tail
            |> Maybe.map \(str, next) -> (char :: str, next)
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
                        'u' -> {- todo -} Debug.todo "Unicode format specifiers are not currently implemented." 
                        _ -> Nothing
        char :: tail ->
            tokenizeString tail
            |> appendStr char
                
                
        
tokenize chars = 
    case chars of
        [] ->
            Just []
            
        head :: tail ->
            case head of
                '(' -> 
                    Just <| LPar :: tokenize tail
                ')' ->
                    Just <| RPar :: tokenize tail
                '"' -> 
                    tokenizeString tail
                    |> Maybe.andThen \(str, rest) -> tokenize rest 
                    |> Maybe.map \tokens -> StringLiteral str :: tokens
                _ ->
                    if isWhitespace head
                    then tokenize tail
                    else
                        let
                            (datum, rest) = 
                                MoreList.splitFirstTrue (\c -> c == '(' || c == ')' || isWhitespace c ) tail
                        in
                        StringLiteral (head :: datum) :: tokenize rest
