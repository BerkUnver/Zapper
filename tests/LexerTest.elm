module LexerTest exposing (..)

import Expect
import Fuzz exposing (list)
import Lexer
import Lexer.Keyword as Keyword
import More.Maybe as Maybe
import SExpr
import Test exposing (..)
import Tokenizer


moduleCharList = 
    Keyword.Module
    |> Keyword.toString
    |> String.toList
        
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
    

    
suite : Test
suite = 
    describe "Lexer"
        [ describe "fromTokenizerToken" <|
            [ test "Invalid string id" <|
                \_ ->
                    String.toList "invalid_id"
                    |> Tokenizer.Id
                    |> SExpr.Literal
                    |> Lexer.fromTokenizerToken
                    |> Expect.equal Nothing 
                    
            , fuzz fuzzValidTokenizerToken "Random valid tokens" <|
                \token ->
                    Lexer.fromTokenizerToken token
                    |> Maybe.isJust
                    |> Expect.true "A single valid tokenizer token token returns (Just (equivalent lexer token))"
            ]
        
        , describe "fromTokenizerTokens"
            [ test "Empty token list" <|
                \_ ->
                    Lexer.fromTokenizerTokens []
                    |> Expect.equal (Just [])
            
            , test "General-case test tokens" <|
                \_ ->
                    [SExpr.LPar, SExpr.Literal <| Tokenizer.Id moduleCharList, SExpr.RPar]
                    |> Lexer.fromTokenizerTokens
                    |> Expect.equal (Just [SExpr.LPar, SExpr.Literal <| Lexer.Keyword Keyword.Module, SExpr.RPar])
                    
            , fuzz (list fuzzValidTokenizerToken) "Many valid tokenizer tokens" <|
                \tokens ->
                    Lexer.fromTokenizerTokens tokens
                    |> Maybe.isJust
                    |> Expect.true "Random valid tokenizer tokens make a valid list of lexer tokens"
            
            , test "Retains order of input tokens" <|
                \_ ->
                    Lexer.fromTokenizerTokens [SExpr.LPar, SExpr.RPar]
                    |> Expect.equal (Just [SExpr.LPar, SExpr.RPar])
            ]
        ]