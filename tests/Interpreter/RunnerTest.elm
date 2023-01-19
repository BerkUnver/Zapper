module Interpreter.RunnerTest exposing (..)

import Ast.Func
import Ast.Instruction exposing (FoldedInstr(..), IfScope, Instruction(..))
import Ast.Module as Module exposing (Ast)
import Dict
import ExampleModules
import Expect
import Fuzz exposing (Fuzzer, bool, float, int, intRange, list, maybe, pair, string)
import Interpreter.Frame exposing (Frame, RuntimeUpdate(..))
import Interpreter.Runner as Runner exposing (FuncReturn(..))
import Interpreter.Val as Val
import More.Maybe as Maybe
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import ValType


emptyAst : Ast
emptyAst = { functions = Dict.empty }


factorial : Int -> Int
factorial i = 
    if i <= 1 then 1 else i * factorial (i - 1) 


suite : Test
suite =
    describe "Module" 
        [ describe "runInstruction"
            [ fuzz2 int int "i32.sub" <|
                \lhs rhs ->
                { locals = Dict.empty, stack = [Val.I32 rhs, Val.I32 lhs]}
                |> Runner.runInstruction I32Sub emptyAst
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = [Val.I32 <| lhs - rhs]})
        
            , test "call" <|
                \_ ->
                let callee = {params = [("p1", ValType.I32), ("p2", ValType.I64)], result = Nothing, locals = [], body = []} in
                let ast = {functions = Dict.singleton "callee" callee} in
                {locals = Dict.empty, stack = [Val.I64 0, Val.I32 0]}
                |> Runner.runInstruction (Call "callee") ast
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = []})
            
            , test "br" <|
                \_ ->
                let frame = {locals = Dict.empty, stack = []} in
                Runner.runInstruction (Br "scope") emptyAst frame
                |> Expect.equal (Ok <| Branch ("scope", frame))
            
            , fuzz int "if" <|
                \int ->
                let 
                    frame : Frame
                    frame = {locals = Dict.empty, stack = [Val.I32 int]} 
                    
                    ifScope : IfScope
                    ifScope = 
                        { label = Nothing
                        , result = Just ValType.I32
                        , thenBlock = [I32Const 3]
                        , elseBlock = [I32Const 4]
                        }
                in
                Runner.runInstruction (If ifScope) emptyAst frame
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = [Val.I32 <| if int /= 0 then 3 else 4]})
                
            ]
        , describe "runScope" <|
            [ fuzz bool "fall out of scope" <|
                \isLoop ->
                let block = { label = Nothing, body = [I32Const 0], result = Just ValType.I32} in
                {locals = Dict.empty, stack = []}
                |> Runner.runScope block isLoop emptyAst
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = [Val.I32 0]})
                
            , test "branch to block" <|
                \_ ->
                let block = {label = Just "block", result = Just ValType.I32, body = [I32Const 2, Br "block"]} in
                {locals = Dict.empty, stack = []}
                |> Runner.runScope block False emptyAst
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = [Val.I32 2]})
            ]
        , describe "runInstructions"
            [ fuzz2 int int "push two integers on the stack, then subtract them." <|
                \lhs rhs ->
                {locals = Dict.empty, stack = []}
                |> Runner.runInstructions [I32Const lhs, I32Const rhs, I32Sub] emptyAst
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = [Val.I32 <| lhs - rhs]})            
            , test "branch" <|
                \_ ->
                let
                    instructions : List Instruction
                    instructions = 
                        [ Block <| 
                            { label = Just "branchTo"
                            , result = Just ValType.I32
                            , body =
                                [ Block <|
                                    { label = Just "branchFrom"
                                    , result = Nothing
                                    , body = [ I32Const 0, Br "branchTo"]
                                    }
                                ] 
                            }
                        ]
                in
                { locals = Dict.empty, stack = []}
                |> Runner.runInstructions instructions emptyAst
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = [Val.I32 0]})                      
            ]
        , describe "runFolded"
            [ fuzz2 int int "addition" <|
                \lhs rhs ->
                let instructions = FInstr (I32Add, [FInstr (I32Const lhs, []), FInstr (I32Const rhs, [])]) in
                {locals = Dict.empty, stack = []}
                |> Runner.runFolded instructions emptyAst
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = [Val.I32 <| lhs + rhs]})
            
            , fuzz2 int int "subtraction" <|
                \lhs rhs -> 
                let instructions = FInstr (I32Sub, [FInstr (I32Const lhs, []), FInstr (I32Const rhs, [])]) in
                {locals = Dict.empty, stack = []}
                |> Runner.runFolded instructions emptyAst
                |> Expect.equal (Ok <| Updated {locals = Dict.empty, stack = [Val.I32 <| lhs - rhs]})
            ]
        , describe "runFunction"
            [ fuzz2 int int "addition" <|
                \lhs rhs ->
                { params = [("lhs", ValType.I32), ("rhs", ValType.I32)]
                , result = Just ValType.I32
                , locals = []
                , body = [LocalGet "lhs", LocalGet "rhs", I32Add]
                }
                |> Runner.runFunction emptyAst [Val.I32 rhs, Val.I32 lhs]
                |> Expect.equal (Ok <| ReturnVal <| Val.I32 <| lhs + rhs)
            
            , fuzz2 int int "subtraction" <|
                \lhs rhs ->
                { params = [("lhs", ValType.I32), ("rhs", ValType.I32)]
                , result = Just ValType.I32
                , locals = []
                , body = [LocalGet "lhs", LocalGet "rhs", I32Sub]               
                }
                |> Runner.runFunction emptyAst [Val.I32 rhs, Val.I32 lhs]
                |> Expect.equal (Ok <| ReturnVal <| Val.I32 <| lhs - rhs)
            
            , test "return" <|
                \_ ->
                { params = []
                , result = Just ValType.I32
                , locals = []
                , body = [I32Const 0, I32Const 1, Return]
                }
                |> Runner.runFunction emptyAst []
                |> Expect.equal (Ok <| ReturnVal <| Val.I32 1)
            ]
        , test "string addition function to code" <|
            \_ ->
            let 
                mod = """
(module
    (func $add (param $lhs i32) (param $rhs i32) (result i32)
        local.get $lhs
        local.get $rhs
        i32.add
    )
    (func $main (result i32)
        i32.const 1
        i32.const 1
        call $add
    )
)
"""            
            in
            Module.parse mod
            |> Result.andThen Runner.runMain
            |> Expect.equal (Ok <| ReturnVal <| Val.I32 2)

        , fuzz (intRange 0 100) "unfolded factorial function" <| 
        -- if the high end of the int-range gets too high there will be a stack overflow exception.
            \int -> 
            let
                modString = """
(module
    (func $fact (param $n i32) (result i32)
        local.get $n
        i32.const 1
        i32.lt_s
        if (result i32)
            i32.const 1
        else
            local.get $n
            i32.const 1
            i32.sub
            call $fact
            local.get $n
            i32.mul
        end
    )
)
"""
            in
            Module.parse modString
            |> Result.andThen (\ast -> 
                ast.functions 
                |> Dict.get "fact" 
                |> Maybe.unwrap
                |> Runner.runFunction ast [Val.I32 int]
            )
            |> Expect.equal (Ok <| ReturnVal <| Val.I32 <| factorial int)
        
        , fuzz (intRange 0 100) "folded factorial function" <|
            \int ->
            Module.parse ExampleModules.factorial
            |> Result.andThen (\ast ->
                ast.functions
                |> Dict.get "fact"
                |> Maybe.unwrap
                |> Runner.runFunction ast [Val.I32 int]
            )
            |> Expect.equal (Ok <| ReturnVal <| Val.I32 <| factorial int)
        ]