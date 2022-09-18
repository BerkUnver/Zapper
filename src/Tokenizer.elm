module Tokenizer exposing (..)

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
                        -- todo : implement unicode format specifiers in strings
                        -- todo : implement hex literals
                        'u' -> Debug.todo "Unicode format specifiers are not currently implemented." 
                        _ -> Nothing
        char :: tail ->
            appendStr char tail


tokenize : List Char -> Maybe (List Token)
tokenize chars = 
    case chars of
        [] ->
            Just []
            
        head :: tail ->
            let concatToken token rest = tokenize rest |> Maybe.map (\tokens -> token :: tokens) in
            case head of
                '(' -> 
                    concatToken LPar tail
                ')' ->
                    concatToken RPar tail
                '"' -> 
                    -- todo : add error message for strings that end and don't have whitespace/parenthesis after
                    tokenizeString tail
                    |> Maybe.andThen (\(str, rest) -> concatToken (StringLiteral str) rest)
                _ ->
                    -- todo : Check for non-7-bit ASCII characters and give error
                    -- todo : Check for line comments
                    -- todo : Check for block comments
                    if isWhitespace head then
                        tokenize tail
                    else
                        let 
                            (datum, rest) = 
                                MoreList.splitFirstTrue (\c -> c == '(' || c == ')' || isWhitespace c ) tail
                        in
                        concatToken (StringLiteral <| head :: datum) rest