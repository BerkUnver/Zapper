module Ast.FuncTest exposing (..)

import Ast.Func as Func exposing (Func)
import Expect
import Fuzz exposing (Fuzzer, list, maybe, pair, string)
import Lexer exposing (Token(..))
import ValTypeFuzz exposing (valType)
import Test exposing (Test, describe, fuzz)


validParam valType name = 
    Scope [Param, Label name, ValType valType]


labelTypeFuzz = pair string valType



paramFuzz = 
    labelTypeFuzz
    |> Fuzz.map (\(label, t) -> {label = label, dataType = t})


resultFuzz = ValTypeFuzz.valType


localFuzz = 
    labelTypeFuzz
    |> Fuzz.map (\(label, t) -> {label = label, dataType = t})
    

funcDeclarationFuzz : Fuzzer Func
funcDeclarationFuzz = 
    ((\params result locals -> {params = params, result = result, locals = locals, body = []})
    |> Fuzz.map3) (list paramFuzz) (maybe resultFuzz) (list localFuzz)


suite : Test
suite = 
    describe "Func" <| 
        [ describe "parseParam" <|
            [ fuzz paramFuzz "general-purpose test-case" <|
                \param ->
                Scope [Param, Label param.label, ValType param.dataType]
                |> Func.parseParam
                |> Expect.equal (Just param)
            ]
        
        , describe "parseParams" <|
            [ fuzz (list paramFuzz) "general test case" <|
                \params ->
                params 
                |> List.foldr (\param into -> Scope [Param, Label param.label, ValType param.dataType] :: into) []
                |> Func.parseParams
                |> Expect.equal (params, [])
            ]
            
        , describe "parseResult" <|
            [ fuzz resultFuzz "general-purpose test case" <|
                \t ->
                Scope [Result, ValType t]
                |> Func.parseResult
                |> Expect.equal (Just t)
            ]
            
        , describe "parseLocal" <|
            [ fuzz localFuzz "general test case" <|
                \local ->
                Scope [Local, Label local.label, ValType local.dataType]
                |> Func.parseLocal
                |> Expect.equal (Just local)
            ]
        
        , describe "parseLocals" <| 
            [ fuzz (list localFuzz) "general" <|
                \locals ->
                locals 
                |> List.foldr (\local into -> Scope [Local, Label local.label, ValType local.dataType] :: into) []
                |> Func.parseLocals 
                |> Expect.equal (locals, [])
            ]
        
        , describe "parse" <|
            [ fuzz funcDeclarationFuzz "empty body, fuzzed declaration." <|
                \func ->
                List.foldr (\local list -> Scope [Local, Label local.label, ValType local.dataType] :: list) [] func.locals -- append the locals
                |> (\tail -> func.result |> Maybe.map (\t -> Scope [Result, ValType t] :: tail) |> Maybe.withDefault tail) -- append the result if it exists
                |> (\tail -> List.foldr (\param list -> Scope [Param, Label param.label, ValType param.dataType] :: list) tail func.params) -- append the params
                |> Func.parse
                |> Expect.equal (Ok func)
            ]
        ]