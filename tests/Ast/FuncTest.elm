module Ast.FuncTest exposing (..)

import Ast.Func as Func exposing (Func)
import Dict
import Expect
import Fuzz exposing (Fuzzer, list, maybe, pair, string)
import Lexer exposing (Token(..))
import More.Dict as Dict
import More.Maybe as Maybe
import ValTypeFuzz exposing (valType)
import Test exposing (Test, describe, fuzz)


validParam valType name = 
    Scope [Param, Label name, ValType valType]


labelTypeFuzz = pair string valType



funcDeclarationFuzz : Fuzzer Func
funcDeclarationFuzz = 
    ((\params result locals ->
        let localVars = Dict.fromList params |> Dict.insertMany locals in 
        { params = List.map Tuple.first params
        , result = result
        , locals = localVars
        , body = []}
    )
    |> Fuzz.map3) (list labelTypeFuzz) (maybe valType) (list (pair string valType))


suite : Test
suite = 
    describe "Func" <| 
        [ describe "parseParam" <|
            [ fuzz labelTypeFuzz "general-purpose test-case" <|
                \param ->
                let (label, t) = param in
                Scope [Param, Label label, ValType t]
                |> Func.parseParam
                |> Expect.equal (Just param)
            ]
        
        , describe "parseParams" <|
            [ fuzz (list labelTypeFuzz) "general test case" <|
                \params ->
                params 
                |> List.foldr (\(label, t) into -> Scope [Param, Label label, ValType t] :: into) []
                |> Func.parseParams
                |> Expect.equal (params, [])
            ]
            
        , describe "parseResult" <|
            [ fuzz valType "general-purpose test case" <|
                \t ->
                Scope [Result, ValType t]
                |> Func.parseResult
                |> Expect.equal (Just t)
            ]
            
        , describe "parseLocal" <|
            [ fuzz labelTypeFuzz "general test case" <|
                \local ->
                let (label, t) = local in
                Scope [Local, Label label, ValType t]
                |> Func.parseLocal
                |> Expect.equal (Just local)
            ]
        
        , describe "parseLocals" <| 
            [ fuzz (list labelTypeFuzz) "general" <|
                \locals ->
                locals 
                |> List.foldr (\(label, t) into -> Scope [Local, Label label, ValType t] :: into) []
                |> Func.parseLocals 
                |> Expect.equal (locals, [])
            ]
        
        , describe "parse" <|
            [ fuzz funcDeclarationFuzz "empty body, fuzzed declaration." <|
                \func ->
                List.foldr (\(label, t) list -> Scope [Local, Label label, ValType t] :: list) [] (Dict.toList func.locals) -- append the locals
                |> (\tail -> func.result |> Maybe.mapWithDefault tail (\t -> Scope [Result, ValType t] :: tail)) -- append the result if it exists
                |> (\tail -> func.params |> List.foldr (\label list -> Scope [Param, Label label, ValType <| Maybe.unwrap <| Dict.get label func.locals] :: list) tail) -- append the params
                |> Func.parse
                |> Expect.equal (Ok func)
            ]
        ]