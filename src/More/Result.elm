module More.Result exposing (..)


unwrap : Result a a -> a
unwrap result =
    case result of
        Ok s -> s
        Err s -> s
        
isOk result = 
    case result of 
        Ok _ -> True
        Err _ -> False