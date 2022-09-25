module Lexer.Instruction.Numeric.F64 exposing (..)


type F64
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