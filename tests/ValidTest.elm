module ValidTest exposing (..)

import Ast.Func exposing (Func)
import Ast.FuncTest as FuncEnv
import Ast.Instruction exposing (FoldedInstr(..), Instruction(..))
import Ast.Module as Module
import Dict
import ExampleModules
import Expect
import Fuzz exposing (Fuzzer, bool, list, maybe, pair, string, triple)
import More.List as List
import More.Maybe as Maybe
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import Valid exposing (FuncEnv, ScopeEnv)
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


addFunc : Func
addFunc =                   
    { params = [("lhs", I32), ("rhs", I32)]
    , result = Just I32
    , locals = []
    , body = [LocalGet "lhs", LocalGet "rhs", I32Add]
    }


suite : Test
suite =
    describe "Valid" 
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
                Valid.op args result env
                |> Expect.equal (Ok finalEnv)
            ]
            
        , 
            let envFuzz = triple (pair string valType) (list valType) bool in
            describe "checkInstruction" 
            [ test "i32.lt_s" <|
                \_ ->
                {dummyEnv | stack = [I32, I32]}
                |> Valid.checkInstruction I32LtS
                |> Expect.equal (Ok {dummyEnv | stack = [I32]})
            
            , fuzz envFuzz "local.get" <|
                \((label, t), stack, unreachable) ->
                let env = {dummyEnv | locals = Dict.singleton label t, stack = stack, unreachable = unreachable} in
                Valid.checkInstruction (LocalGet label) env
                |> Expect.equal (Ok {env | stack = t :: stack})
            
            , fuzz envFuzz "local.set" <|
                \((label, t), stack, unreachable) ->
                let env = {dummyEnv | locals = Dict.singleton label t, stack = t :: stack, unreachable = unreachable} in
                Valid.checkInstruction (LocalSet label) env
                |> Expect.equal (Ok {env | stack = stack})
            
            , fuzz envFuzz "local.tee" <|
                \((label, t), stack, unreachable) ->
                let env = {dummyEnv | locals = Dict.singleton label t, stack = t :: stack, unreachable = unreachable} in
                Valid.checkInstruction (LocalTee label) env
                |> Expect.equal (Ok env)
            
            , fuzz (maybe valType) "return" <|
                \result ->
                let env = {dummyEnv | result = result, stack = List.fromMaybe result} in
                Valid.checkInstruction Return env
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
                Valid.checkInstruction (Br label) env
                |> Expect.equal (Ok {env | stack = [], unreachable = True})
            
            , fuzz2 string FuncEnv.funcFuzz "call" <|
                \label fn ->
                let env = {dummyEnv | functions = Dict.singleton label fn, stack = List.reverseMap Tuple.second fn.params} in
                Valid.checkInstruction (Call label) env
                |> Expect.equal (Ok {env | stack = List.fromMaybe fn.result})
            ]
            
        , describe "checkInstructions" -- this should be good enough
            [ test "two adds" <|
                \_ -> 
                let env = {dummyEnv | stack = [I32, I32, I32]} in
                Valid.checkInstructions [I32Add, I32Add] env
                |> Expect.equal (Ok {env | stack = [I32]})
                
            , test "f32Eq then add" <|
                \_ ->
                let env = {dummyEnv | stack = [F32, F32, I32]} in
                Valid.checkInstructions [F32Eq, I32Add] env
                |> Expect.equal (Ok {env | stack = [I32]})
            
            , test "return from if" <|
                \_ ->
                let 
                    ifScope = 
                        { label = Nothing
                        , result = Nothing -- result can be nothing because returning in the if statement causes the stack to be unwound
                        , thenBlock = [I32Const 1, Return]
                        , elseBlock = []
                        }
                    instructions = [If ifScope, I32Const 0] 
                    env = {dummyEnv | stack = [I32], result = Just I32}
                in
                Valid.checkInstructions instructions env
                |> Expect.equal (Ok env)
            ]
            
        , describe "checkFunction" 
            [ test "simple addition function" <|
                \_ ->
                { params = [("lhs", I32), ("rhs", I32)]
                , result = Just I32
                , locals = []
                , body = [LocalGet "lhs", LocalGet "rhs", I32Add]
                }
                |> Valid.checkFunc Dict.empty
                |> Expect.equal (Ok ())
            
            , fuzz2 (list valType) (maybe valType) "return discards arguments" <|
                \body result ->
                let 
                    getDefault t =
                        case t of
                            I32 -> I32Const 0
                            I64 -> I64Const 0
                            F32 -> F32Const 0
                            F64 -> F64Const 0
                in
                { params = []
                , result = result
                , locals = []
                , body = 
                    (case result of 
                        Nothing -> [Return]
                        Just resultVal -> [getDefault resultVal, Return]
                    ) |> \i -> List.foldl (\t s -> getDefault t :: s) i body 
                }
                |> Valid.checkFunc Dict.empty
                |> Expect.equal (Ok ())
            ]
        
        , describe "checkScope" <|
            [ test "empty scope" <|
                \_ ->
                Valid.checkScope {label = Nothing, result = Nothing, body = []} False dummyEnv
                |> Expect.equal (Ok dummyEnv)
            ]
            
        , describe "checkFolded" <|
        
            [ test "i32.lt_s folded" <|
                \_ ->
                let env = {dummyEnv | locals = Dict.singleton "x" I32} in
                Valid.checkFolded (FInstr (I32LtS, [FInstr (LocalGet "x", []),  FInstr (I32Const 1, [])])) env
                |> Expect.equal (Ok {env | stack = [I32]})
            
            , test "calling folded function" <|
                \_ -> 
                let
                    env = {dummyEnv | stack = [I32, I32], functions = Dict.singleton "add" addFunc}
                    instruction = FInstr (Call "add", [])
                in
                Valid.checkFolded instruction env
                |> Expect.equal (Ok {env | stack = [I32]})
            ]
            
        , test "factorial" <|
            \_ ->
            Module.parse ExampleModules.factorial
            |> Result.andThen (\ast ->
                Dict.get "fact" ast.functions 
                |> Maybe.unwrap
                |> Valid.checkFunc ast.functions
            )
            |> Expect.equal (Ok ())
        ]