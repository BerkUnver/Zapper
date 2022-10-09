module More.List exposing (..)

splitFirstTrue predicate list =
    case list of
        [] ->
            ([], [])
        head :: tail ->
            if predicate head then
                ([], list)
            else
                let
                    (left, right) = splitFirstTrue predicate tail
                in
                    (head :: left, right)

firstTrue predicate list = 
    case list of 
        [] -> 
            []
        head :: tail ->
            if predicate head
            then list
            else firstTrue predicate tail


mapUntilNothing predicate list =
    case list of 
        [] -> ([], [])
        head :: tail ->
            case predicate head of 
                Nothing -> ([], list)
                Just element ->
                    let 
                        (left, right) = mapUntilNothing predicate tail 
                    in
                        (element :: left, right)
                        
                    
    

count predicate list = 
    case list of 
        [] -> 0
        head :: tail ->
            (if predicate head then 1 else 0) + count predicate tail


contains elem list = 
    case list of
        [] -> False
        head :: tail ->
            if head == elem
            then True
            else contains elem tail

       
appendN num element list =
    if num <= 0
    then list
    else element :: appendN (num - 1) element list
    

firstJust func list = 
    case list of
        [] -> Nothing
        head :: tail ->
            case func head of 
                Nothing -> firstJust func tail
                result -> result
                
tryAll func list =
    case list of
        head :: tail ->
            func head 
            |> Maybe.andThen (\result -> 
                tryAll func tail 
                |> Maybe.map (\rest -> result :: rest))
        
        [] -> Just []
        