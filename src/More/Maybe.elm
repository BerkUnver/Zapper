module More.Maybe exposing (..)

isJust maybe = 
    case maybe of
        Just _ -> True
        Nothing -> False
        
appendIfJust maybeList element =
    maybeList
    |> Maybe.map (\elements -> element :: elements)