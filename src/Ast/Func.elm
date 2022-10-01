module Ast.Func exposing (..)

import Lexer
import NumType exposing (NumType)
import More.List as List
import SExpr exposing (SExpr)
 

-- todo : Add anonymous params, locals, and functions that can be accessed only by index.
-- todo : Add type aliases in function name

type alias Param =
    { name : String
    , dataType : NumType
    }


type alias Local = 
    { name : String
    , dataType : NumType
    }
    

type alias Func = 
    { name : String
    , params : List Param
    , results : List NumType
    , locals : List Local
    , body : () -- todo : Add body of function
    }


parseParam : SExpr Lexer.Token -> Maybe Param
parseParam param =
    case param of
        SExpr.List 
            [ SExpr.Atom Lexer.Param
            , SExpr.Atom (Lexer.Var name)
            , SExpr.Atom (Lexer.NumType t)
            ] -> 
            Just { name = name, dataType = t }
        
        _ -> Nothing
        

parseParams : List (SExpr Lexer.Token) -> (List Param, List (SExpr Lexer.Token))
parseParams funcSExpr = List.mapUntilNothing parseParam funcSExpr 
        
        
parseResult : SExpr Lexer.Token -> Maybe NumType
parseResult result =
    case result of
        SExpr.List [SExpr.Atom Lexer.Param, SExpr.Atom (Lexer.NumType t)] -> 
            Just t
        _ -> Nothing
        

parseResults : List (SExpr Lexer.Token) -> (List NumType, List (SExpr Lexer.Token))
parseResults funcSExpr = List.mapUntilNothing parseResult funcSExpr


parseLocal : SExpr Lexer.Token -> Maybe Local
parseLocal local = 
    case local of 
        SExpr.List [SExpr.Atom Lexer.Param, SExpr.Atom (Lexer.Var var), SExpr.Atom (Lexer.NumType t)] ->
            Just {name = var, dataType = t}
        _ -> Nothing


parseLocals : List (SExpr Lexer.Token) -> (List Local, List (SExpr Lexer.Token))
parseLocals func = List.mapUntilNothing parseLocal func


parse : List (SExpr Lexer.Token) -> Maybe Func
parse func = 
    case func of
        SExpr.Atom Lexer.Func :: SExpr.Atom (Lexer.Var name) :: paramsResultsLocalsBody ->
            let
                (params, resultsLocalsBody) = parseParams paramsResultsLocalsBody
                (results, localsBody) = parseResults resultsLocalsBody
                (locals, _) = parseLocals localsBody
            in
                { name = name
                , params = params
                , results = results
                , locals = locals
                , body = ()
                }
                |> Just -- This is just like this until parsing the body of the function is added
        _ -> Nothing