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
    
        

parseDeclaration : Token -> Result String Func
parseDeclaration sExpr =
    -- todo : add import, export, type alias
    case sExpr of
        Scope scope ->
            Func.parse scope
        _ -> Err (Debug.toString sExpr ++ " is not a valid module-level declaration.")
            

parse : Token -> Result String Ast
parse sExpr = 
    case sExpr of
        Scope (Module :: declarations) ->
            declarations
            |> List.allOk parseDeclaration
            |> Result.map (\functions -> {functions = functions})
        _ -> Err "The top-level construct is not a module."


parse360 : String -> Result String String
parse360 str =
    String.toList str
    |> Tokenizer.tokenize
    |> Result.andThen Lexer.lex
    |> Result.andThen parse
    |> Result.map toString