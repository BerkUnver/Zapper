module More.Maybe exposing (..)

isJust maybe = 
    case maybe of
        Just _ -> True
        Nothing -> False
        
andThenCons element maybeList =
    maybeList
    |> Maybe.map (\elements -> element :: elements)