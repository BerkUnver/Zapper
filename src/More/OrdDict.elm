module More.OrdDict exposing (OrdDict, empty, append, keyGet, idxGet)

import Array exposing (Array)
import Dict exposing (Dict)
import More.Dict as Dict


type OrdDict comparable val = OrdDict {array : Array comparable, dict : Dict comparable val }

empty : OrdDict comparable val
empty = OrdDict {array = Array.empty, dict = Dict.empty}

append : comparable -> val -> OrdDict comparable val -> Maybe (OrdDict comparable val)
append key val (OrdDict d) =
    Dict.insertUnique key val d.dict
    |> Maybe.map (\x -> OrdDict {array = Array.append (Array.fromList [key]) d.array, dict = x})

keyGet : comparable -> OrdDict comparable val -> Maybe val 
keyGet key (OrdDict d) = 
    Dict.get key d.dict
    
idxGet : Int -> OrdDict comparable val -> Maybe val 
idxGet idx (OrdDict d) =
    Array.get idx d.array
    |> Maybe.andThen (\key -> Dict.get key d.dict ) -- should always return Just ...