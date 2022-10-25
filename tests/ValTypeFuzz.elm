module ValTypeFuzz exposing (..)

import Fuzz
import ValType exposing (ValType(..))


valTypeFuzz = 
    [I32, I64, F32, F64]
    |> List.map Fuzz.constant
    |> Fuzz.oneOf