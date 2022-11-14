module Ast.Module exposing (..)

import Ast.Func as Func exposing (Func)
import Dict exposing (Dict)
import Format
import Lexer exposing (Token(..))
import More.List as List
import Tokenizer

    
type alias Ast = 
    { functions : Dict String Func }
    -- todo : add export, import, type-use


toString : Ast -> String
toString ast = 
    ast.functions
    |> Dict.toList
    |> List.map (\(label, func) -> Format.indent ("(func $" ++ label ++ " " ++ Func.toString func ++ "\n)")) -- todo : Don't put a space if there are no params or results.
    |> String.join "\n\n"
    |> \x -> "(module\n" ++ x ++ "\n)"
    

insertFunc : Token -> Ast -> Result String Ast
insertFunc token ast =
    case token of
        -- todo : add import, export, type alias, 
        Scope (Lexer.Func :: Label label :: func) ->
            Func.parse func
            |> Result.andThen (\parsedFunc ->
                if Dict.get label ast.functions == Nothing then 
                    Ok {ast | functions = Dict.insert label parsedFunc ast.functions}
                else
                    Err <| "Duplicate function declaration: $" ++ label
            )
        Scope (Lexer.Func :: _) ->
            Err "Anonymous functions are currently not supported."    -- todo : anonymous functions
        _ -> 
            Err <| Debug.toString token ++ " is not a valid module-level declaration."
            

parse : Token -> Result String Ast
parse sExpr = 
    case sExpr of
        Scope (Module :: declarations) ->
            List.foldLeftUntilErr insertFunc declarations {functions = Dict.empty}
        _ ->
            Err "The top-level construct is not a module."


parse360 : String -> Result String String
parse360 str =
    String.toList str
    |> Tokenizer.tokenize
    |> Result.andThen Lexer.lex
    |> Result.andThen parse
    |> Result.map toString