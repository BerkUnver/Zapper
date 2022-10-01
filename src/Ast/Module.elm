module Ast.Module exposing (..)

import Ast.Func as Func exposing (Func)
import Lexer
import SExpr exposing (SExpr)

    

type alias Ast = 
    { functions : List Func }
    -- todo : add export, import, type alias



parseDeclaration sExpr =
    -- todo : add import, export, type alias
    case sExpr of
        SExpr.List (SExpr.Atom Lexer.Func :: SExpr.Atom (Lexer.Var name) :: func) ->
            let 
                (params, resultsAndBody) = Func.parseParams func
                (results, body) = Func.parseResults resultsAndBody 
            in
            { name = name
            , params = params
            , results = results
            , body = ()
            }
            -- todo : be able to parse body
            |> Ok
        _ -> Err "Valid function declarations consist of the function keyword, a function name, parameters, return types, and a body."
            
        
parse sExpr = 
    case sExpr of
        SExpr.List Lexer.Module :: declarations ->
            
        _ -> Err "All WASM files must consist of a module enclosed by parenthesis."
        