module Ast.Func exposing (..)

import Ast.Instruction as Instruction exposing (Instruction)
import Format
import Lexer exposing (Token(..))
import More.String as String
import ValType exposing (ValType)
import More.List as List
 

-- todo : Add anonymous params, locals, and functions that can be accessed only by index.
-- todo : Add type aliases in function name

type alias Param =
    { label : String
    , dataType : ValType
    }


paramToString : Param -> String
paramToString param = 
    "(param $" ++ param.label ++ " " ++ ValType.toString param.dataType ++ ")"
    
    
type alias Local = 
    { label : String
    , dataType : ValType
    }
    
    
localToString : Local -> String
localToString local =
    "(local $" ++ local.label ++ " " ++ ValType.toString local.dataType ++ ")"
    

type alias Func = 
    { label : String
    , params : List Param
    , result : List ValType
    , locals : List Local
    , body : List Instruction
    }


resultToString : ValType -> String
resultToString valType = 
    "(result " ++ ValType.toString valType ++ ")" 


toString : Func -> String
toString func = 
    "(func $" ++ func.label 
    ++ String.joinWithFirst " " (List.map paramToString func.params)
    ++ String.joinWithFirst " " (List.map resultToString func.results)
    ++ String.joinWithFirst Format.newLineTab (List.map localToString func.locals)
    ++ (Format.indentBody <| String.joinWithFirst "\n" <| List.map Instruction.toString func.body)
    ++ "\n)"
            
            
parseParam : Lexer.Token -> Maybe Param
parseParam param =
    case param of
        Scope [Lexer.Param, Var name, ValType t] -> 
            Just { label = name, dataType = t }
        _ -> Nothing
        

parseParams : List Lexer.Token -> (List Param, List Lexer.Token)
parseParams funcSExpr = List.mapUntilNothing parseParam funcSExpr 
        
        
parseResult : Lexer.Token -> Maybe ValType
parseResult result =
    case result of
        Scope [Lexer.Result, Lexer.ValType t] -> 
            Just t
        _ -> Nothing
        

parseResults : List Lexer.Token -> (List ValType, List Lexer.Token)
parseResults funcSExpr = List.mapUntilNothing parseResult funcSExpr


parseLocal : Lexer.Token -> Maybe Local
parseLocal local = 
    case local of 
        Scope [Lexer.Local, Var var, ValType t] ->
            Just {label = var, dataType = t}
        _ -> Nothing


parseLocals : List Lexer.Token -> (List Local, List Lexer.Token)
parseLocals func = List.mapUntilNothing parseLocal func


parse : List Lexer.Token -> Result String Func
parse func = 
    case func of
        Lexer.Func :: Var name :: paramsResultsLocalsBody ->
            let
                (params, resultsLocalsBody) = parseParams paramsResultsLocalsBody
                (results, localsBody) = parseResults resultsLocalsBody
                (locals, body) = parseLocals localsBody
            in
            Instruction.parse body
            |> Result.map (\parsedBody ->
            { label = name
            , params = params
            , results = results
            , locals = locals
            , body = parsedBody
            })
        _ -> Err "Not a function."