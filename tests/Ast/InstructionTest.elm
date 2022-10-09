module Ast.InstructionTest exposing (..)

import Ast.Instruction as Instruction exposing (Instruction(..))
import Expect
import Format
import Lexer exposing (Token(..))
import Test exposing (Test, describe, test)


suite : Test
suite = 
    describe "Instruction"
        [ describe "parseSingle"
            [ test "local.get" <|
                \_ ->
                    Instr "local.get" :: Var "" :: []
                    |> Instruction.parseSingle
                    |> Expect.equal (Just (LocalGet "", []))
            -- most of the rest are trivial, not worth testing every one
            
            , test "block" <|
                \_ ->
                    Scope [Instr "block", Var ""] :: []
                    |> Instruction.parseSingle
                    |> Expect.equal (Just (Block {label = "", body = []}, []))
            , test "if" <|
                \_ ->
                    Scope [Instr "if", Var "", Scope [Instr "then"], Scope [Instr "else"]] :: []
                    |> Instruction.parseSingle
                    |> Expect.equal (Just (If {label = "", thenBlock = [], elseBlock = []}, []))
            ]
        , describe "toString" <|
            [test "loop" <|
                \_ ->
                    Loop {label = "name", body = [I32Add]}
                    |> Instruction.toString
                    |> Expect.equal ("(loop $name\n" ++ Format.tab ++ Instruction.toString I32Add ++ "\n)")
            
            , test "if" <|
                \_ ->
                    If {label = "name", thenBlock = [I32Add], elseBlock = [I32Sub]}
                    |> Instruction.toString
                    |> Expect.equal (
                    "(if $name\n" 
                    ++ Format.tab ++ "(then\n" 
                    ++ Format.tab ++ Format.tab ++ Instruction.toString I32Add ++ "\n"
                    ++ Format.tab ++ ")\n"
                    ++ Format.tab ++ "(else\n"
                    ++ Format.tab ++ Format.tab ++ Instruction.toString I32Sub ++ "\n"
                    ++ Format.tab ++ ")\n"
                    ++ ")")
                 
            ]
        ]