module Ast.FuncTest exposing (..)

import Ast.Func as Func
import Expect
import Fuzz exposing (Fuzzer, list)
import Lexer exposing (Token(..))
import ValTypeFuzz
import Test exposing (Test, describe, fuzz, test)


validParam valType name = 
    Scope [Param, Var name, ValType valType]


labelTypeFuzz = 
    Fuzz.tuple (Fuzz.string, ValTypeFuzz.valTypeFuzz)


paramFuzz = 
    labelTypeFuzz
    |> Fuzz.map (\(label, t) -> {label = label, dataType = t})


resultFuzz = ValTypeFuzz.valTypeFuzz


localFuzz = 
    labelTypeFuzz
    |> Fuzz.map (\(label, t) -> {label = label, dataType = t})
    

funcDeclarationFuzz = 
    ((\label params results locals -> {label = label, params = params, results = results, locals = locals, body = []})
    |> Fuzz.map4) Fuzz.string (list paramFuzz) (list resultFuzz) (list localFuzz)


suite : Test
suite = 
    describe "Func" <| 
        [ describe "parseParam" <|
            [ fuzz paramFuzz "general-purpose test-case" <|
                \param ->
                Scope [Param, Var param.label, ValType param.dataType]
                |> Func.parseParam
                |> Expect.equal (Just param)
            ]
        
        , describe "parseParams" <|
            [ fuzz (list paramFuzz) "general test case" <|
                \params ->
                params 
                |> List.foldr (\param into -> Scope [Param, Var param.label, ValType param.dataType] :: into) []
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
        
        , describe "parseResults" <|
            [ fuzz (list resultFuzz) "general test case" <|
                \results ->
                results 
                |> List.foldr (\t into -> Scope [Result, ValType t] :: into) []
                |> Func.parseResults
                |> Expect.equal (results, [])
            ]
            
        , describe "parseLocal" <|
            [ fuzz localFuzz "general test case" <|
                \local ->
                Scope [Local, Var local.label, ValType local.dataType]
                |> Func.parseLocal
                |> Expect.equal (Just local)
            ]
        
        , describe "parseLocals" <| 
            [ fuzz (list localFuzz) "general" <|
                \locals ->
                locals 
                |> List.foldr (\local into -> Scope [Local, Var local.label, ValType local.dataType] :: into) []
                |> Func.parseLocals 
                |> Expect.equal (locals, [])
            ]
        
        , describe "parse" <|
            [ fuzz funcDeclarationFuzz "empty body, fuzzed declaration." <|
                \func ->
                List.foldr (\local list -> Scope [Local, Var local.label, ValType local.dataType] :: list) [] func.locals
                |> (\tail -> List.foldr (\result list -> Scope [Result, ValType result] :: list) tail func.results)
                |> (\tail -> List.foldr (\param list -> Scope [Param, Var param.label, ValType param.dataType] :: list) tail func.params)
                |> (\tail -> Func :: Var func.label :: tail)
                |> Func.parse
                |> Expect.equal (Ok func)
            ]
        ]