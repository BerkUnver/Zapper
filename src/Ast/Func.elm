module Ast.Func exposing (..)

import Ast.Instruction as Instruction exposing (Instruction)
import Format
import Lexer exposing (Token(..))
import More.String as String
import NumType exposing (NumType)
import More.List as List
 

-- todo : Add anonymous params, locals, and functions that can be accessed only by index.
-- todo : Add type aliases in function name

type alias Param =
    { name : String
    , dataType : NumType
    }


paramToString : Param -> String
paramToString param = 
    "(param $" ++ param.name ++ " " ++ NumType.toString param.dataType ++ ")"
    
    
type alias Local = 
    { name : String
    , dataType : NumType
    }
    
    
localToString : Local -> String
localToString local =
    "(local $" ++ local.name ++ " " ++ NumType.toString local.dataType ++ ")"
    

type alias Func = 
    { name : String
    , params : List Param
    , results : List NumType
    , locals : List Local
    , body : List Instruction
    }


resultToString : NumType -> String
resultToString numType = 
    "(result " ++ NumType.toString numType ++ ")" 


toString : Func -> String
toString func = 
    "(func $" ++ func.name 
    ++ String.joinWithFirst " " (List.map paramToString func.params)
    ++ String.joinWithFirst " " (List.map resultToString func.results)
    ++ String.joinWithFirst Format.newLineTab (List.map localToString func.locals)
    ++ (Format.indentBody <| String.joinWithFirst "\n" <| List.map Instruction.toString func.body)
    ++ "\n)"
            
            
parseParam : Lexer.Token -> Maybe Param
parseParam param =
    case param of
        Scope [Lexer.Param, Var name, NumType t] -> 
            Just { name = name, dataType = t }
        _ -> Nothing
        

parseParams : List Lexer.Token -> (List Param, List Lexer.Token)
parseParams funcSExpr = List.mapUntilNothing parseParam funcSExpr 
        
        
parseResult : Lexer.Token -> Maybe NumType
parseResult result =
    case result of
        Scope [Lexer.Result, Lexer.NumType t] -> 
            Just t
        _ -> Nothing
        

parseResults : List Lexer.Token -> (List NumType, List Lexer.Token)
parseResults funcSExpr = List.mapUntilNothing parseResult funcSExpr


parseLocal : Lexer.Token -> Maybe Local
parseLocal local = 
    case local of 
        Scope [Lexer.Local, Var var, NumType t] ->
            Just {name = var, dataType = t}
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
            { name = name
            , params = params
            , results = results
            , locals = locals
            , body = parsedBody
            })
        _ -> Err "Not a function."