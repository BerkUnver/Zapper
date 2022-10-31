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
                    [Instr "block", Label ""]
                    |> Instruction.parseFolded
                    |> Expect.equal (Ok <| FBlock {label = Just "", result = Nothing, body = []})
            
            , test "empty if" <|
                \_ ->
                    [Instr "if", Label "label", Scope [Instr "then"], Scope [Instr "else"]]
                    |> Instruction.parseFolded
                    |> Expect.equal (Ok <| FIf {label = Just "label", result = Nothing, folded = Nothing, thenBlock = [], elseBlock = []})
            ]
         
        , describe "parseSingle" <|
            [ test "i32.add" <|
                \_ ->
                Instr "i32.add" :: []
                |> Instruction.parseSingle
                |> Expect.equal (Ok (I32Add, []))
                
            , test "local.get" <|
                \_ ->
                    Instr "local.get" :: Label "" :: []
                    |> Instruction.parseSingle
                    |> Expect.equal (Ok (LocalGet "", []))
            -- most of the rest are trivial, not worth testing every one
                    
            , test "empty unfolded block" <|
                \_ ->
                    [Instr "block", Label "", Instr "end"]
                    |> Instruction.parseSingle
                    |> Expect.equal (Ok (Block {label = Just "", result = Nothing, body = []}, []))
                        
            
            , test "empty unfolded if with label" <|
                \_ ->
                   [Instr "if", Label "label", Instr "else", Instr "end"]
                   |> Instruction.parseSingle
                   |> Expect.equal (Ok (If {label = Just "label", result = Nothing, thenBlock = [], elseBlock = []}, []))
            , test "empty unfolded if without label" <|
                \_ ->
                    [Instr "if", Instr "else", Instr "end"]
                    |> Instruction.parseSingle
                    |> Expect.equal (Ok (If {label = Nothing, result = Nothing, thenBlock = [], elseBlock = []}, []))
                   
           
            ]
         
        , describe "foldedToString" <|
            [ test "folded" <| 
                \_ ->
                    FInstr (I32Add, [FInstr (LocalGet "0", []), FInstr (LocalGet "1", [])])
                    |> Instruction.foldedToString
                    |> Expect.equal "(i32.add (local.get $0) (local.get $1))"
            
            , test "if with label" <|
                \_ -> 
                    FIf {label = Just "label", folded = Nothing, result = Nothing, thenBlock = [], elseBlock = []}
                    |> Instruction.foldedToString
                    |> Expect.equal (
                        "(if $label"
                        ++ Format.newLineTab ++ "(then"
                        ++ Format.newLineTab ++ ")"
                        ++ Format.newLineTab ++ "(else"
                        ++ Format.newLineTab ++ ")\n)"
                    )
            , test "if without label" <|
                \_ ->
                    FIf {label = Nothing, result = Nothing, folded = Nothing, thenBlock = [], elseBlock = []}
                    |> Instruction.foldedToString
                    |> Expect.equal (
                        "(if"
                        ++ Format.newLineTab ++ "(then"
                        ++ Format.newLineTab ++ ")"
                        ++ Format.newLineTab ++ "(else"
                        ++ Format.newLineTab ++ ")\n)"
                    )
            ]
            
        , describe "toString" <|
            [ test "loop" <|
                \_ ->
                    Loop {label = Just "name", result = Nothing, body = [I32Add]}
                    |> Instruction.toString
                    |> Expect.equal (
                    "loop $name" 
                    ++ Format.newLineTab ++ Instruction.toString I32Add 
                    ++ "\nend")
            
            , test "if" <|
                -- todo : change the unfolded version
                \_ ->
                    If {label = Just "name", result = Nothing, thenBlock = [I32Add], elseBlock = [I32Sub]}
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
                [Instr "local.get", Label local1, Instr "local.get", Label local2]
                |> Instruction.parse
                |> Expect.equal (Ok [LocalGet local1, LocalGet local2])
                
            , test "exact failure case of test" <|
                \_ -> [Instr "local.get", Label "0", Instr "local.get", Label "1", Instr "i32.add"]
                |> Instruction.parse
                |> Expect.equal (Ok [LocalGet "0", LocalGet "1", I32Add])
            ]
        ]