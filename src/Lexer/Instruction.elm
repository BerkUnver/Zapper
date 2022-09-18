module Lexer.Instruction exposing (..)


import Lexer.Instruction.F32 exposing (F32)
import Lexer.Instruction.F64 exposing (F64)
import Lexer.Instruction.I32 exposing (I32)
import Lexer.Instruction.I64 exposing (I64)


type Numeric
    = I32 I32
    | I64 I64
    | F32 F32
    | F64 F64 
    
type Parametric
    = Drop
    | Select


type Variable
    = LocalGet
    | LocalSet
    | LocalTee
    | GlobalGet
    | GlobalSet


type Control
    = Nop
    | Unreachable
    | Block
    | Loop
    | If
    | Else
    | End
    | Branch
    | BranchIf
    -- todo : br_table
    | Return
    | Call
    -- todo : call_indirect
    

type Instruction 
    = Numeric Numeric
    -- todo : Vector
    -- todo : Reference
    | Parametric Parametric
    | Variable Variable
    -- todo : Table
    -- todo : Memory
    | Control Control