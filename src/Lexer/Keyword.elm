module Lexer.Keyword exposing (..)
import Lexer.Instruction exposing (Instruction)
import Lexer.NumType as NumType exposing (NumType)

type Keyword 
    = Module
    | Func
    | Param 
    | Result
    | NumType NumType
    | Instruction Instruction


toString keyword = 
    case keyword of
        Module -> "module"
        Func -> "func" 
        Param -> "param"
        Result -> "result"
        NumType t -> NumType.toString t
        Instruction _ -> Debug.todo "No instructions yet" -- todo : Add instructions


fromCharList chars = 
    if chars == String.toList "module" then Just Module
    else if chars == String.toList "func" then Just Func
    else if chars == String.toList "param" then Just Param
    else if chars == String.toList "result" then Just Result
    else 
        case NumType.fromCharList chars of
            Nothing -> Nothing
            Just num -> Just <| NumType num
