module LexerTest exposing (..)

import Expect
import Lexer
import Test exposing (..)
import Tokenizer
    
suite : Test
suite = 
    describe "Lexer"
        [ describe "parse"
            [ test "Single literal" <| 
                \_ ->
                    [Tokenizer.String []]
                    |> Lexer.lex
                    |> Expect.equal (Ok <| Lexer.String "")  
            
            , test "List with single literal" <|
                \_ ->
                    [Tokenizer.LPar, Tokenizer.String [], Tokenizer.RPar]
                    |> Lexer.lex
                    |> Expect.equal (Ok <| Lexer.Scope [Lexer.String ""])
                    
            , test "Recursive parse" <|
                \_ ->
                    [Tokenizer.LPar, Tokenizer.LPar, Tokenizer.String [], Tokenizer.RPar, Tokenizer.LPar, Tokenizer.RPar, Tokenizer.RPar]
                    |> Lexer.lex
                    |> Expect.equal (Ok <| Lexer.Scope [Lexer.Scope [Lexer.String ""], Lexer.Scope []])
            ]
        ]