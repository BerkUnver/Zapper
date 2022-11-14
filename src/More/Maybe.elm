module More.Maybe exposing (..)

isJust maybe = 
    case maybe of
        Just _ -> True
        Nothing -> False
        
andThenCons element maybeList =
    maybeList
    |> Maybe.map (\elements -> element :: elements)
    

mapWithDefault element func maybe =  
    case maybe of 
        Just x -> func x
        Nothing -> element
        
unwrap maybe =
    case maybe of
        Just x -> x
        Nothing -> Debug.todo "Unsafe unwrap."