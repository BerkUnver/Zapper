module Ast.Module exposing (..)

import Ast.Func as Func exposing (Func)
import Ast.Instruction as Instruction
import Dict exposing (Dict)
import Format
import Lexer exposing (Token(..))
import More.List as List
import More.Maybe as Maybe
import More.String as String
import Tokenizer

    

type alias Ast = 
    { functions : Dict String Func }
    -- todo : add export, import, type-use


toString : Ast -> String
toString ast =
    let 
        funcToString label func = 
            let params = List.map Func.paramToString func.params |> String.join " " in
            "(func $" ++ label
            ++ (if String.isEmpty params then "" else " " ++ params)
            ++ (func.result |> Maybe.mapWithDefault "" (\x -> " " ++ Func.resultToString x))
            ++ String.joinWithFirst Format.newLineTab (List.map Func.localToString func.locals)
            ++ (Format.indentBody <| String.joinWithFirst "\n" <| List.map Instruction.toString func.body)
            ++ "\n)"
    in
    ast.functions
    |> Dict.toList
    |> List.map (\(label, func) -> funcToString label func |> Format.indent)
    |> String.join "\n\n"
    |> \x -> "(module\n" ++ x ++ "\n)"
    

insertFunc : Token -> Ast -> Result String Ast
insertFunc token ast =
    case token of
        Scope (Lexer.Func :: Label label :: tail) ->   -- todo : add import, export, type alias
            Func.parse tail
            |> Result.andThen (\parsedFunc ->
                if Dict.get label ast.functions == Nothing then 
                    Ok {ast | functions = Dict.insert label parsedFunc ast.functions}
                else
                    Err <| "Duplicate function declaration: $" ++ label
            )
            
        _ -> 
            Err <| Debug.toString token ++ " is not a valid module-level declaration."
            

parseTokens : Token -> Result String Ast
parseTokens sExpr = 
    case sExpr of
        Scope (Module :: declarations) ->
            List.foldLeftUntilErr insertFunc declarations {functions = Dict.empty}
        _ ->
            Err "The top-level construct is not a module."


parse : String -> Result String Ast
parse str = 
    String.toList str
    |> Tokenizer.tokenize
    |> Result.andThen Lexer.lex
    |> Result.andThen parseTokens


parse360 : String -> Result String String
parse360 str =
    parse str
    |> Result.map toString