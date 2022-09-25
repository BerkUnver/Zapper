module Lexer.Instruction.Numeric.I32 exposing (..)

-- todo : add all instructions
-- stopped at 0xA7 on the table here
-- https://webassembly.github.io/spec/core/appendix/index-instructions.html
type I32
    = Const
    | EqZero
    | Eq
    | NotEq
    | LessThanSigned
    | GreaterThanSigned
    | LessThanOrEqlSigned
    | LessThanOrEqUnsigned
    | GreaterThanOrEqSigned
    | GreaterThanOrEqUnsigned
    | CountLeadingZeros
    | CountTrailingZeros
    | PopulationCount -- count number of ones in int
    | Add
    | Sub
    | Mul
    | DivSigned
    | DivUnsigned
    | RemainderSigned
    | RemainderUnsigned
    | And
    | Or
    | XOr
    | ShiftLeft
    | ShiftRightSigned
    | ShiftRightUnsigned
    | RotateLeft
    | RotateRight
