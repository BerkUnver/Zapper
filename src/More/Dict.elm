module More.Dict exposing (..)

import Dict exposing (Dict)
import More.List as List


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


insertNew : comparable -> value -> Dict comparable value -> Maybe (Dict comparable value)
insertNew key value dict =
    if Dict.member key dict then
        Nothing
    else
        Just <| Dict.insert key value dict
        

insertManyNew : List (comparable, value) -> Dict comparable value -> Maybe (Dict comparable value)
insertManyNew keyValPairs dict =
    List.foldLeftUntilNothing (\(key, val) dictionary -> insertNew key val dictionary) keyValPairs dict
    

insertMany : List (comparable, value) -> Dict comparable value -> Dict comparable value
insertMany list dict =
    case list of
        (k, v) :: tail ->
            Dict.insert k v dict
            |> insertMany tail
        
        [] -> dict