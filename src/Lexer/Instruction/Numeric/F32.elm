module Lexer.Instruction.Numeric.F32 exposing (..)


type F32
    = Const
    | Eq
    | NotEq
    | LessThan
    | GreaterThan
    | LessThanOrEq
    | GreaterThanOrEq
    | Abs
    | Neg
    | Ceil
    | Floor
    | Truncate
    | Nearest
    | Sqrt
    | Add
    | Sub
    | Mul
    | Div
    | Min
    | Max
    | CopySign