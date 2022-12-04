module More.Maybe exposing (..)

isJust maybe = 
    case maybe of
        Just _ -> True
        Nothing -> False
        
andThenCons element maybeList =
    maybeList
    |> Maybe.map (\elements -> element :: elements)
    

unwrap maybe =
    case maybe of
        Just x -> x
        Nothing -> Debug.todo "failed to unwrap maybe."
        

mapWithDefault : b -> (a -> b) -> Maybe a -> b
mapWithDefault default func maybe =
    case maybe of
        Nothing -> default
        Just x -> func x