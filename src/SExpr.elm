module SExpr exposing (..)

type Token t 
    = Literal t 
    | LPar
    | RPar


type SExpr t 
    = Atom t 
    | List (List (SExpr t))


parse tokens = 
    case tokens of 
        [] -> 
            Nothing
        Literal t :: [] -> 
            Just <| Atom t
        LPar :: tail ->
            let 
                parseTail tailTokens = 
                    case tailTokens of
                        [] ->
                            Nothing
                            
                        RPar :: restTokens ->
                            Just ([], restTokens)
                        
                        Literal t :: restTokens -> 
                            parseTail restTokens
                            |> Maybe.map (\(listSExpr, rest) -> (Atom t :: listSExpr, rest))
                        
                        LPar :: restTokens ->
                            parseTail restTokens
                            |> Maybe.andThen (\(headListSExpr, nextTokens) -> 
                                parseTail nextTokens 
                                 |> Maybe.map (\(listSExpr, leftoverTokens) -> 
                                    (List headListSExpr :: listSExpr, leftoverTokens)))
                        
            in
                case parseTail tail of
                    Just (listSExpr, []) -> Just <| List listSExpr
                    _ -> Nothing
        _ -> 
            Nothing
