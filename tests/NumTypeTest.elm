module NumTypeTest exposing (..)

import Fuzz
import NumType exposing (NumType(..))


numTypeFuzz = 
    [I32, I64, F32, F64]
    |> List.map Fuzz.constant
    |> Fuzz.oneOf