module Ast.FuncTest exposing (..)

import Ast.Func as Func
import Expect
import Fuzz exposing (list)
import Lexer exposing (Token(..))
import NumTypeTest
import Test exposing (Test, describe, fuzz, test)


validParam numType name = 
    Scope [Param, Var name, NumType numType]


nameTypeFuzz = 
    Fuzz.tuple (Fuzz.string, NumTypeTest.numTypeFuzz)


paramFuzz = 
    nameTypeFuzz
    |> Fuzz.map (\(name, t) -> {name = name, dataType = t})


resultFuzz = NumTypeTest.numTypeFuzz


localFuzz = 
    nameTypeFuzz
    |> Fuzz.map (\(name, t) -> {name = name, dataType = t})
    
funcDeclarationFuzz = 
    ((\name params results locals -> {name = name, params = params, results = results, locals = locals, body = []})
    |> Fuzz.map4) Fuzz.string (list paramFuzz) (list resultFuzz) (list localFuzz)

suite : Test
suite = 
    describe "Func" <| 
        [ describe "parseParam" <|
            [ fuzz paramFuzz "general-purpose test-case" <|
                \param ->
                Scope [Param, Var param.name, NumType param.dataType]
                |> Func.parseParam
                |> Expect.equal (Just param)
            ]
        
        , describe "parseParams" <|
            [ fuzz (list paramFuzz) "general test case" <|
                \params ->
                params 
                |> List.foldr (\param into -> Scope [Param, Var param.name, NumType param.dataType] :: into) []
                |> Func.parseParams
                |> Expect.equal (params, [])
            ]
            
        , describe "parseResult" <|
            [ fuzz resultFuzz "general-purpose test case" <|
                \t ->
                Scope [Result, NumType t]
                |> Func.parseResult
                |> Expect.equal (Just t)
            ]
        
        , describe "parseResults" <|
            [ fuzz (list resultFuzz) "general test case" <|
                \results ->
                results 
                |> List.foldr (\t into -> Scope [Result, NumType t] :: into) []
                |> Func.parseResults
                |> Expect.equal (results, [])
            ]
            
        , describe "parseLocal" <|
            [ fuzz localFuzz "general test case" <|
                \local ->
                Scope [Local, Var local.name, NumType local.dataType]
                |> Func.parseLocal
                |> Expect.equal (Just local)
            ]
        
        , describe "parseLocals" <| 
            [ fuzz (list localFuzz) "general" <|
                \locals ->
                locals 
                |> List.foldr (\local into -> Scope [Local, Var local.name, NumType local.dataType] :: into) []
                |> Func.parseLocals 
                |> Expect.equal (locals, [])
            ]
        
        , describe "parse" <|
            [ fuzz funcDeclarationFuzz "empty body, fuzzed declaration." <|
                \func ->
                List.foldr (\local list -> Scope [Local, Var local.name, NumType local.dataType] :: list) [] func.locals
                |> (\tail -> List.foldr (\result list -> Scope [Result, NumType result] :: list) tail func.results)
                |> (\tail -> List.foldr (\param list -> Scope [Param, Var param.name, NumType param.dataType] :: list) tail func.params)
                |> (\tail -> Func :: Var func.name :: tail)
                |> Func.parse
                |> Expect.equal (Just func)
            
            -- , test "strange failure case" <|
            --     \_ ->
            --     Func :: Var "folded" :: Scope []
            ]
        ]