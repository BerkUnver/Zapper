module LexerTest exposing (..)

import Expect
import Lexer
import Test exposing (..)

suite : Test
suite = 
    describe "Lexer"
        [ describe "fromTokenizerTokens"
            [ test "Empty token list" <|
                \_ ->
                    Lexer.fromTokenizerTokens []
                    |> Expect.equal (Just [])
            ]
        ]