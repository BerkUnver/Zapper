module Ast.Module exposing (..)

import Ast.Func as Func exposing (Func)
import Format
import Lexer exposing (Token(..))
import More.List as List
import Tokenizer

    

type alias Ast = 
    { functions : List Func }
    -- todo : add export, import, type alias


toString ast = 
    ast.functions
    |> List.map (Func.toString >> Format.indent)
    |> String.join "\n\n"
    |> \x -> "(module\n" ++ x ++ "\n)"
    
        

parseDeclaration sExpr =
    -- todo : add import, export, type alias
    case sExpr of
        Scope scope ->
            Func.parse scope
        _ -> Nothing
            
    
parse sExpr = 
    case sExpr of
        Scope (Module :: declarations) ->
            declarations
            |> List.tryAll parseDeclaration
            |> Maybe.map (\functions -> {functions = functions})
        _ -> Nothing
        

parse360 str =
    String.toList str
    |> Tokenizer.tokenize
    |> Maybe.andThen Lexer.lex
    |> Maybe.andThen parse
    |> Maybe.map toString