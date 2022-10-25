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

             
allJust func list =
    case list of
        head :: tail ->
            func head 
            |> Maybe.andThen (\result -> 
                allJust func tail 
                |> Maybe.map (\rest -> result :: rest))
        
        [] -> Just []
        

allOk func list = 
    case list of
        head :: tail ->
            func head
            |> Result.andThen (\result -> 
                allOk func tail 
                |> Result.map (\rest -> result :: rest))
        
        [] -> Ok []
        

partialEq list1 list2 = 
    case (list1, list2) of
        (head1 :: tail1, head2 :: tail2) ->
            if head1 == head2 then 
                partialEq tail1 tail2 
            else 
                False
        _ -> True
        
        
popAllEq list1 list2 = 
    let pair = (list1, list2) in
    case pair of
        (head1 :: tail1, head2 :: tail2) ->
            if head1 == head2 then
                popAllEq tail1 tail2
            else
                pair
        _ -> pair


headAndTail list = 
    case list of 
        head :: tail ->
            Just (head, tail)
        [] -> Nothing