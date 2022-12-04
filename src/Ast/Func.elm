module Ast.Func exposing (..)

import Ast.Instruction as Instruction exposing (Instruction)
import Lexer exposing (Token(..))
import ValType exposing (ValType)
import More.List as List
 
-- todo : Add anonymous params, locals, and functions that can be accessed only by index.
-- todo : Add type aliases in function name
    
paramToString : (String, ValType) -> String
paramToString (label, t) = 
    "(param $" ++ label ++ " " ++ ValType.toString t ++ ")"
    
    
localToString : (String, ValType) -> String
localToString (label, t) =
    "(local $" ++ label ++ " " ++ ValType.toString t ++ ")"
    

resultToString : ValType -> String
resultToString valType = 
    "(result " ++ ValType.toString valType ++ ")"
            
            
parseParam : Lexer.Token -> Maybe (String, ValType)
parseParam param =
    case param of
        Scope [Lexer.Param, Label label, ValType t] -> 
            Just (label, t)
        _ -> Nothing
        

parseParams : List Lexer.Token -> (List (String, ValType), List Lexer.Token)
parseParams funcSExpr = List.mapUntilNothing parseParam funcSExpr 
        
        
parseResult : Lexer.Token -> Maybe ValType
parseResult result =
    case result of
        Scope [Lexer.Result, Lexer.ValType t] -> 
            Just t
        _ -> Nothing
        

parseLocal : Lexer.Token -> Maybe (String, ValType)
parseLocal local = 
    case local of 
        Scope [Lexer.Local, Label label, ValType t] ->
            Just (label, t)
        _ -> Nothing


parseLocals : List Lexer.Token -> (List (String, ValType), List Lexer.Token)
parseLocals func = List.mapUntilNothing parseLocal func


type alias Func =
    { params : List (String, ValType)
    , result : Maybe ValType
    , locals : List (String, ValType)
    , body : List Instruction
    }


parse : List Lexer.Token -> Result String Func
parse func = 
    let
        (params, resultLocalsBody) = parseParams func
        (result, localsBody) = Instruction.parseResult resultLocalsBody
        (locals, body) = parseLocals localsBody
    in
    Instruction.parse body
    |> Result.map (\parsedBody ->
    { params = params
    , result = result
    , locals = locals
    , body = parsedBody
    })
