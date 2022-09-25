module Lexer.Instruction.Numeric exposing (..)


import Lexer.Instruction.Numeric.F32 exposing (F32)
import Lexer.Instruction.Numeric.F64 exposing (F64)
import Lexer.Instruction.Numeric.I32 exposing (I32)
import Lexer.Instruction.Numeric.I64 exposing (I64)


type Numeric
    = I32 I32
    | I64 I64
    | F32 F32
    | F64 F64 
    