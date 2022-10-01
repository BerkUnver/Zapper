module LexerTest exposing (..)

import Expect
import Lexer
import SExpr
import Test exposing (..)
import Tokenizer

moduleCharList = 
    Lexer.Module
    |> Lexer.toString
    |> String.toList

{-  
validTokens = 
    [ SExpr.LPar
    , SExpr.RPar
    , SExpr.Literal <| Tokenizer.String <| String.toList "valid string literal"
    , SExpr.Literal <| Tokenizer.Id moduleCharList
    ]

fuzzValidTokenizerToken = 
    validTokens
    |> List.map Fuzz.constant
    |> Fuzz.oneOf
-}  

    
suite : Test
suite = 
    describe "Lexer"
        [ describe "fromTokenizerToken" <|
            [ test "Random string turns into instruction" <|
                \_ ->
                    let 
                        str = "invalid_id"
                        chars = String.toList str
                    in
                    
                    Tokenizer.Id chars
                    |> SExpr.Literal
                    |> Lexer.fromTokenizerToken
                    |> Expect.equal (SExpr.Literal <| Lexer.Instr str)
            ]
        
        , describe "fromTokenizerTokens"
            [ test "Empty token list" <|
                \_ ->
                    Lexer.fromTokenizerTokens []
                    |> Expect.equal []
            
            , test "General-case test tokens" <|
                \_ ->
                    [SExpr.LPar, SExpr.Literal <| Tokenizer.Id moduleCharList, SExpr.RPar]
                    |> Lexer.fromTokenizerTokens
                    |> Expect.equal [SExpr.LPar, SExpr.Literal <| Lexer.Module, SExpr.RPar]
                    
            , test "Retains order of input tokens" <|
                \_ ->
                    Lexer.fromTokenizerTokens [SExpr.LPar, SExpr.RPar]
                    |> Expect.equal [SExpr.LPar, SExpr.RPar]
            ]
        ]