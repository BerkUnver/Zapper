﻿module Lexer.NumType exposing (..)

import Lexer.Word as Word

type NumType
    = I32
    | I64
    | F32
    | F64

toString t =
    case t of 
        I32 -> "i32"
        I64 -> "i64"
        F32 -> "f32"
        F64 -> "f64"

fromString chars = 
    if chars == "i32" then Just I32
    else if chars ==  "i64" then Just I64
    else if chars == "f32" then Just F32
    else if chars == "i64" then Just I64
    else Nothing