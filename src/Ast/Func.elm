module Ast.Func exposing (..)

import Ast.Instruction as Instruction exposing (Instruction)
import Dict exposing (Dict)
import Format
import Lexer exposing (Token(..))
import More.Dict as Dict
import More.Maybe as Maybe
import More.String as String
import ValType exposing (ValType)
import More.List as List
 
-- todo : Add anonymous params, locals, and functions that can be accessed only by index.
-- todo : Add type aliases in function name


paramToString : String -> ValType -> String
paramToString label t = 
    "(param $" ++ label ++ " " ++ ValType.toString t ++ ")"
    
    
localToString : (String, ValType) -> String
localToString (label, t) =
    "(local $" ++ label ++ " " ++ ValType.toString t ++ ")"
    

type alias Func = 
    { params : List String
    , result : Maybe ValType
    , locals : Dict String ValType
    , body : List Instruction
    }


resultToString : ValType -> String
resultToString valType = 
    "(result " ++ ValType.toString valType ++ ")" 


toString : Func -> String
toString func =
    String.join " " (List.map (\p -> func.locals |> Dict.get p |> Maybe.unwrap |> paramToString p) func.params) 
    ++ (func.result |> Maybe.mapWithDefault "" (\x -> " " ++ resultToString x))
    ++ String.joinWithFirst Format.newLineTab (func.locals |> Dict.toList |> List.map localToString)
    ++ (Format.indentBody <| String.joinWithFirst "\n" <| List.map Instruction.toString func.body)

            
parseParam : Lexer.Token -> Maybe (String, ValType)
parseParam param =
    case param of
        Scope [Lexer.Param, Label name, ValType t] -> 
            Just (name, t)
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
        Scope [Lexer.Local, Label var, ValType t] ->
            Just (var, t)
        _ -> Nothing


parseLocals : List Lexer.Token -> (List (String, ValType), List Lexer.Token)
parseLocals func = List.mapUntilNothing parseLocal func


parse : List Lexer.Token -> Result String Func
parse func = 
    let
        (params, resultLocalsBody) = parseParams func
        (result, localsBody) = Instruction.parseResult resultLocalsBody
        (locals, body) = parseLocals localsBody
    in
    Instruction.parse body
    |> Result.andThen (\parsedBody -> 
        Dict.empty
        |> Dict.insertManyNew params
        |> Maybe.andThen (Dict.insertManyNew locals)
        |> Result.fromMaybe "Duplicate local / param declaration."
        |> Result.map (\localVars ->
            { params = List.map Tuple.first params
            , result = result
            , locals = localVars
            , body = parsedBody
            }
        )
    )