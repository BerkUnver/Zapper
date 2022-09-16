﻿module Keyword exposing (..)

import NumType exposing (NumType)
type Keyword 
    = Module
    | Func
    | Param 
    | Result
    | NumType NumType
    | Export
    
toString keyword = 
    case keyword of
        Module -> "module"
        Func -> "func" 
        Param -> "param"
        Result -> "result"
        NumType t -> NumType.toString t
        Export -> "export"

toCharList = toString >> String.toList

consumeKeyword chars =
    
