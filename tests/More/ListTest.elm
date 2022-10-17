module More.ListTest exposing (..)

import Expect
import Fuzz exposing (list, maybe, unit)
import More.List as List
import More.Maybe as Maybe
import Test exposing (Test, describe, fuzz, test)
suite : Test
suite = 
    describe "List" 
        [ describe "tryAll"
            [ test "Empty list" <|
                \_ -> 
                    List.allJust (\_ -> Just ()) []
                    |> Expect.equal (Just [])
            
            , fuzz (list (maybe unit)) "Hits a nothing mid-list" <|
                \list ->
                    let result = List.allJust identity list in
                    if List.contains Nothing list then
                        Expect.equal Nothing result
                    else
                        Maybe.isJust result
                        |> Expect.true "Result has elements."
            
            , test "Applies in the correct order" <|
                \_ ->
                    [Just 1, Just 2, Just 3]
                    |> List.allJust identity
                    |> Expect.equal (Just [1, 2, 3])
            ]
        ]