module Lexer.Instruction.Control exposing (..)


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