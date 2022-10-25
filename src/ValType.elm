module ValType exposing (..)


type ValType
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


fromString str =
    case str of
        "i32" -> Just I32
        "i64" -> Just I64
        "f32" -> Just F32
        "f64" -> Just F64
        _ -> Nothing 
        
        
fromChars chars = 
    String.fromList chars
    |> fromString
