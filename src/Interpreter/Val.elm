module Interpreter.Val exposing (..)

import ValType exposing (ValType)


type Val
    = I32 Int
    | I64 Int
    | F32 Float
    | F64 Float


toString : Val -> String
toString val =
    case val of 
        I32 i -> "I32 " ++ String.fromInt i
        I64 i -> "I64 " ++ String.fromInt i
        F32 f -> "F32 " ++ String.fromFloat f
        F64 f -> "F64 " ++ String.fromFloat f


getType : Val -> ValType
getType val = 
    case val of 
        I32 _ -> ValType.I32
        I64 _ -> ValType.I64
        F32 _ -> ValType.F32
        F64 _ -> ValType.F64


typeToString : Val -> String
typeToString val = 
    getType val
    |> ValType.toString


ofType : ValType -> Val -> Bool
ofType t val = getType val == t


sameType : Val -> Val -> Bool
sameType v1 v2 = getType v1 == getType v2


default : ValType -> Val
default t =
    case t of 
        ValType.I32 -> I32 0
        ValType.I64 -> I64 0
        ValType.F32 -> F32 0
        ValType.F64 -> F64 0