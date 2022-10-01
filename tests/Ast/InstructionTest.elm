module Ast.InstructionTest exposing (..)

import Ast.Instruction as Instruction exposing (Instruction(..))
import Expect
import Lexer exposing (Token(..))
import SExpr exposing (SExpr(..))
import Test exposing (Test, describe, test)


suite : Test
suite = 
    describe "Instruction"
        [ describe "parseSingle"
            [ test "local.get" <|
                \_ ->
                    Atom (Instr "local.get") :: Atom (Var "") :: []
                    |> Instruction.parseSingle
                    |> Expect.equal (Just (LocalGet "", []))
            -- most of the rest are trivial, not worth testing every one
            
            , test "block" <|
                \_ ->
                    SExpr.List [Atom (Instr "block"), Atom (Var "")] :: []
                    |> Instruction.parseSingle
                    |> Expect.equal (Just (Block {name = "", body = []}, []))
            ]
        ]