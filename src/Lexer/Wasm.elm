﻿module WasmToken exposing (..)

import Keyword exposing (Keyword)
    
type Token 
    = Keyword Keyword
    | U32 Int
    | I32 Int
    | Float Float
    | String (List Char)
    | Id (List Char)
    | LPar
    | RPar
    | Reserved


        
                        
        
        
tokenize chars =
    case chars of
        [] -> []
        head :: tail ->
            case head of 
                '(' -> LPar :: tokenize tail
                ')' -> RPar :: tokenize tail
                '"' -> -- consume string
                '$' -> -- consume id
                _ ->
                    if 
                        Keyword.to
                    
                