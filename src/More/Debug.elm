module More.Debug exposing (..)

logAny item =
    Debug.toString item 
    |> Debug.log
    |> \_ -> item 
    