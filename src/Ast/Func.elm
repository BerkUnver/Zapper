module Ast.Func exposing (..)

import Lexer
import Lexer.Keyword as Keyword
import Lexer.NumType exposing (NumType)
import MoreList
import SExpr exposing (SExpr)
 


type alias Param =
    { name : Maybe (List Char)
    , dataType : NumType
    }

    
type alias Func = 
    { name : String
    , params : List Param
    , returnTypes : List NumType
    , body : () -- todo : Add body of function
    }


parseParam : SExpr Lexer.WasmLiteral -> Maybe Param
parseParam param =
    case param of
        SExpr.List [SExpr.Atom (Lexer.Keyword Keyword.Param), SExpr.Atom (Lexer.Id name), SExpr.Atom (Lexer.Keyword (Keyword.NumType t))] -> 
            Just { name = Just name, dataType = t }
       
        SExpr.List [ SExpr.Atom (Lexer.Keyword Keyword.Param), SExpr.Atom (Lexer.Keyword (Keyword.NumType t))] ->
            Just { name = Nothing, dataType = t}
        
        _ -> Nothing
        

parseParams : List (SExpr Lexer.WasmLiteral) -> (List Param, List (SExpr Lexer.WasmLiteral))
parseParams funcSExpr = MoreList.mapUntilNothing parseParam funcSExpr 
        
        
parseResult : SExpr Lexer.WasmLiteral -> Maybe NumType
parseResult result =
    case result of
        SExpr.List [SExpr.Atom (Lexer.Keyword Keyword.Param), SExpr.Atom (Lexer.Keyword (Keyword.NumType t))] -> 
            Just t
        _ -> Nothing
        

parseResults : List (SExpr Lexer.WasmLiteral) -> (List NumType, List (SExpr Lexer.WasmLiteral))
parseResults funcSExpr = MoreList.mapUntilNothing parseResult funcSExpr