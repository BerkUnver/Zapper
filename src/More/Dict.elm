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
            
insertUnique key val dict = 
    if Dict.member key dict then
        Nothing
    else
        Just <| Dict.insert key val dict
        
insertManyUnique list dict = 
    case list of 
        (key, val) :: tail ->
            insertUnique key val dict
            |> Maybe.andThen (insertManyUnique tail)
        [] ->
            Just dict