module TypeCheckerTest exposing (..)

import Ast.Instruction exposing (Instruction(..))
import Dict
import Expect
import Fuzz exposing (Fuzzer, bool, list, maybe, pair, string, triple)
import More.List as List
import Test exposing (Test, describe, fuzz, fuzz3, test)
import TypeChecker exposing (FuncEnv, ScopeEnv)
import ValType exposing (ValType(..))
import ValTypeFuzz exposing (valType)


dummyEnv : FuncEnv
dummyEnv = 
    { result = Nothing
    , functions = Dict.empty
    , locals = Dict.empty
    , stack = []
    , scopes = []
    , unreachable = False
    }


scopeEnvFuzz : Fuzzer ScopeEnv    
scopeEnvFuzz = 
    Fuzz.map4 
        (\label result isLoop unreachable -> {label = label, result = result, isLoop = isLoop, unreachable = unreachable}) 
        (maybe string) (maybe valType) bool bool


suite : Test
suite =
    describe "TypeChecker" 
        [ describe "op" 
            [ fuzz3 (pair (list valType) (list valType)) (maybe valType) bool "random op succeeds" <|
                \(args, stack) result unreachable ->
                let 
                    env = {dummyEnv | stack = List.appendReverse args stack, unreachable = unreachable}
                    finalEnv =
                        case result of
                            Just r -> {env | stack = r :: stack}
                            Nothing -> {env | stack = stack}
                in
                TypeChecker.op args result env
                |> Expect.equal (Ok finalEnv)
            ]
            
        , 
            let envFuzz = triple (pair string valType) (list valType) bool in
            describe "checkInstr" 
            [ fuzz envFuzz "local.get" <|
                \((label, t), stack, unreachable) ->
                let env = {dummyEnv | locals = Dict.singleton label t, stack = stack, unreachable = unreachable} in
                TypeChecker.checkInstr (LocalGet label) env
                |> Expect.equal (Ok {env | stack = t :: stack})
            
            , fuzz envFuzz "local.set" <|
                \((label, t), stack, unreachable) ->
                let env = {dummyEnv | locals = Dict.singleton label t, stack = t :: stack, unreachable = unreachable} in
                TypeChecker.checkInstr (LocalSet label) env
                |> Expect.equal (Ok {env | stack = stack})
            
            , fuzz envFuzz "local.tee" <|
                \((label, t), stack, unreachable) ->
                let env = {dummyEnv | locals = Dict.singleton label t, stack = t :: stack, unreachable = unreachable} in
                TypeChecker.checkInstr (LocalTee label) env
                |> Expect.equal (Ok env)
            
            , fuzz (maybe valType) "return" <|
                \result ->
                let env = {dummyEnv | result = result, stack = List.fromMaybe result} in
                TypeChecker.checkInstr Return env
                |> Expect.equal (Ok {env | stack = [], unreachable = True})
          
            -- br_if is practically identical, except the unreachable flag is not set afterwards
            , fuzz3 string (maybe valType) (pair bool bool) "br" <|
                \label result (isLoop, unreachable) ->
                let 
                    scopeEnv = {label = Just label, result = result, isLoop = isLoop, unreachable = unreachable}
                    env = 
                        { dummyEnv 
                        | scopes = [scopeEnv]
                        , stack = if scopeEnv.isLoop then [] else List.fromMaybe scopeEnv.result
                        } 
                in
                TypeChecker.checkInstr (Br label) env
                |> Expect.equal (Ok {env | stack = [], unreachable = True})
            
            , fuzz3 string (list valType) (maybe valType) "call" <|
                \label params result ->
                let env = {dummyEnv | functions = Dict.singleton label (params, result), stack = List.reverse params} in
                TypeChecker.checkInstr (Call label) env
                |> Expect.equal (Ok {env | stack = List.fromMaybe result})
            ]
        
        , describe "checkInstructions" -- this should be good enough
            [ test "two adds" <|
                \_ -> 
                let env = {dummyEnv | stack = [I32, I32, I32]} in
                TypeChecker.checkInstructions [I32Add, I32Add] env
                |> Expect.equal (Ok {env | stack = [I32]})
                
            , test "f32Eq then add" <|
                \_ ->
                let env = {dummyEnv | stack = [F32, F32, I32]} in
                TypeChecker.checkInstructions [F32Eq, I32Add] env
                |> Expect.equal (Ok {env | stack = [I32]})
            ]
        
        , describe "checkScope" <|
            [ test "empty scope" <|
                \_ ->
                TypeChecker.checkScope {label = Nothing, result = Nothing, body = []} False dummyEnv
                |> Expect.equal (Ok dummyEnv)
            ]
        ]