module Lexer.Instruction exposing (..)

import Lexer.Instruction.Control exposing (Control)
import Lexer.Instruction.Numeric exposing (Numeric)
import Lexer.Instruction.Parametric exposing (Parametric)
import Lexer.Instruction.Variable exposing (Variable)


type Instruction 
    = Numeric Numeric
    -- todo : Vector
    -- todo : Reference
    | Parametric Parametric
    | Variable Variable
    -- todo : Table
    -- todo : Memory
    | Control Control