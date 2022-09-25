module SEpxrTest exposing (..)

import Expect
import SExpr 
import Test exposing (Test, describe, test)

suite : Test
suite  = describe "SExpr" 
    [ describe "parse" 
        [ test "Single literal" <| 
            \_ ->
                [SExpr.Literal ()]
                |> SExpr.parse
                |> Expect.equal (Just <| SExpr.Atom ())  
        
        , test "List with single literal" <|
            \_ ->
                [SExpr.LPar, SExpr.Literal (), SExpr.RPar]
                |> SExpr.parse
                |> Expect.equal (Just <| SExpr.List <| [SExpr.Atom ()])
                
        , test "Recursive parse" <|
            \_ ->
                [SExpr.LPar, SExpr.LPar, SExpr.Literal (), SExpr.RPar, SExpr.LPar, SExpr.RPar, SExpr.RPar]
                |> SExpr.parse
                |> Expect.equal (Just <| SExpr.List <| [SExpr.List <| [SExpr.Atom ()], SExpr.List []])
        ]
    ]
