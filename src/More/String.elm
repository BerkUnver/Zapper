module More.String exposing (..)

joinWithFirst str list = 
    list 
    |> List.map (\x -> str ++ x)
    |> String.concat