module Ast.Func exposing (..)

import Ast.Instruction as Instruction exposing (Instruction)
import Lexer exposing (Token(..))
import ValType exposing (ValType)
import More.List as List
 
-- todo : Add anonymous params, locals, and functions that can be accessed only by index.
-- todo : Add type aliases in function name

type alias Local = 
    { label : String
    , dataType : ValType
    }
    

paramToString : Local -> String
paramToString param = 
    "(param $" ++ param.label ++ " " ++ ValType.toString param.dataType ++ ")"
    
    
localToString : Local -> String
localToString local =
    "(local $" ++ local.label ++ " " ++ ValType.toString local.dataType ++ ")"
    

resultToString : ValType -> String
resultToString valType = 
    "(result " ++ ValType.toString valType ++ ")"
            
            
parseParam : Lexer.Token -> Maybe Local
parseParam param =
    case param of
        Scope [Lexer.Param, Label name, ValType t] -> 
            Just { label = name, dataType = t }
        _ -> Nothing
        

parseParams : List Lexer.Token -> (List Local, List Lexer.Token)
parseParams funcSExpr = List.mapUntilNothing parseParam funcSExpr 
        
        
parseResult : Lexer.Token -> Maybe ValType
parseResult result =
    case result of
        Scope [Lexer.Result, Lexer.ValType t] -> 
            Just t
        _ -> Nothing
        

parseLocal : Lexer.Token -> Maybe Local
parseLocal local = 
    case local of 
        Scope [Lexer.Local, Label var, ValType t] ->
            Just {label = var, dataType = t}
        _ -> Nothing


parseLocals : List Lexer.Token -> (List Local, List Lexer.Token)
parseLocals func = List.mapUntilNothing parseLocal func


type alias Func = -- TODO : make it illegal to have duplicate names for things!
    { params : List Local
    , result : Maybe ValType
    , locals : List Local
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
