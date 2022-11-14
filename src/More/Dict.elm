module More.Dict exposing (..)

import Dict exposing (Dict)

allUnique : List (comparable, value) -> Maybe (Dict comparable value)
allUnique list = 
    case list of 
        [] -> 
            Just Dict.empty
        
        (key, value) :: tail ->
            allUnique tail
            |> Maybe.andThen (\dict -> 
                if Dict.get key dict == Nothing then 
                    Just (Dict.insert key value dict) 
                else 
                    Nothing
            ) 