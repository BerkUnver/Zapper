module Lexer.Instruction.Variable exposing (..)


type Variable
    = LocalGet
    | LocalSet
    | LocalTee
    | GlobalGet
    | GlobalSet