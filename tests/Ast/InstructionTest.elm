module Ast.InstructionTest exposing (..)

import Ast.Instruction as Instruction exposing (FoldedInstr(..), Instruction(..))
import Expect
import Format
import Fuzz exposing (string, tuple)
import Lexer exposing (Token(..))
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Instruction"
        [ describe "parseFolded" <|
            [ test "empty block" <| 
                \_ ->
                    [Instr "block", Var ""]
                    |> Instruction.parseFolded
                    |> Expect.equal (Just (FBlock {label = "", body = []}))
            
            , test "empty if" <|
                \_ ->
                    [Instr "if", Var "", Scope [Instr "then"], Scope [Instr "else"]]
                    |> Instruction.parseFolded
                    |> Expect.equal (Just (FIf {label = "", folded = [], thenBlock = [], elseBlock = []}))
            ]
         
        , describe "parseSingle" <|
            [ test "i32.add" <|
                \_ ->
                Instr "i32.add" :: []
                |> Instruction.parseSingle
                |> Expect.equal (Just (I32Add, []))
                
            , test "local.get" <|
                \_ ->
                    Instr "local.get" :: Var "" :: []
                    |> Instruction.parseSingle
                    |> Expect.equal (Just (LocalGet "", []))
            -- most of the rest are trivial, not worth testing every one
                    
            , test "empty unfolded block" <|
                \_ ->
                    [Instr "block", Var "", Instr "end"]
                    |> Instruction.parseSingle
                    |> Expect.equal (Just (Block {label = "", body = []}, []))
                        
            
            , test "empty unfolded if" <|
                \_ ->
                   [Instr "if", Var "", Instr "else", Instr "end"]
                   |> Instruction.parseSingle
                   |> Expect.equal (Just (If {label = "", thenBlock = [], elseBlock = []}, []))
            ]
         
        , describe "foldedToString" <|
            [ test "folded" <| 
                \_ ->
                    FInstr (I32Add, [FInstr (LocalGet "0", []), FInstr (LocalGet "1", [])])
                    |> Instruction.foldedToString
                    |> Expect.equal "(i32.add (local.get $0) (local.get $1))"
            
            , test "if" <|
                \_ -> 
                    FIf {label = "label", folded = [], thenBlock = [], elseBlock = []}
                    |> Instruction.foldedToString
                    |> Expect.equal (
                        "(if $label"
                        ++ Format.newLineTab ++ "(then"
                        ++ Format.newLineTab ++ ")"
                        ++ Format.newLineTab ++ "(else"
                        ++ Format.newLineTab ++ ")\n)"
                    )
            ]
            
        , describe "toString" <|
            [ test "loop" <|
                \_ ->
                    Loop {label = "name", body = [I32Add]}
                    |> Instruction.toString
                    |> Expect.equal (
                    "loop $name" 
                    ++ Format.newLineTab ++ Instruction.toString I32Add 
                    ++ "\nend")
            
            , test "if" <|
                -- todo : change the unfolded version
                \_ ->
                    If {label = "name", thenBlock = [I32Add], elseBlock = [I32Sub]}
                    |> Instruction.toString
                    |> Expect.equal (
                    "if $name" 
                    ++ Format.newLineTab ++ Instruction.toString I32Add
                    ++ "\nelse"
                    ++ Format.newLineTab ++ Instruction.toString I32Sub
                    ++ "\nend")
            ]
            
        , describe "parse" <|
            [ fuzz (tuple (string, string)) "multiple local.get" <|
                \(local1, local2) ->
                [Instr "local.get", Var local1, Instr "local.get", Var local2]
                |> Instruction.parse
                |> Expect.equal (Just [LocalGet local1, LocalGet local2])
                
            , test "exact failure case of test" <|
                \_ -> [Instr "local.get", Var "0", Instr "local.get", Var "1", Instr "i32.add"]
                |> Instruction.parse
                |> Expect.equal (Just [LocalGet "0", LocalGet "1", I32Add])
            ]
        ]