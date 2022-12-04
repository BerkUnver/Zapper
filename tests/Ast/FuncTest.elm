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


funcFuzz : Fuzzer Func
funcFuzz = 
    ((\params result locals -> {params = params, result = result, locals = locals, body = []})
    |> Fuzz.map3) (list labelTypeFuzz) (maybe valType) (list labelTypeFuzz)


suite : Test
suite = 
    describe "Func" <| 
        [ describe "parseParam" <|
            [ fuzz labelTypeFuzz "general-purpose test-case" <|
                \(label, t) ->
                Scope [Param, Label label, ValType t]
                |> Func.parseParam
                |> Expect.equal (Just (label, t))
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
                \(label, t) ->
                Scope [Local, Label label, ValType t]
                |> Func.parseLocal
                |> Expect.equal (Just (label, t))
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
            [ fuzz funcFuzz "empty body, fuzzed declaration." <|
                \func ->
                List.foldr (\(label, t) list -> Scope [Local, Label label, ValType t] :: list) [] func.locals -- append the locals
                |> (\tail -> func.result |> Maybe.map (\t -> Scope [Result, ValType t] :: tail) |> Maybe.withDefault tail) -- append the result if it exists
                |> (\tail -> List.foldr (\(label, t) list -> Scope [Param, Label label, ValType t] :: list) tail func.params) -- append the params
                |> Func.parse
                |> Expect.equal (Ok func)
            ]
        ]