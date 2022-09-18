module TokenizerTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, tuple, char, int, list, string)
import MoreMaybe
import Test exposing (..)
import Tokenizer exposing (Token(..))

--- this is just a debug function, please use valid debug characters
legalizeChars escapeChar chars =
    List.foldr (
        \char charList ->
            if char == '\\' then
                '\\' :: escapeChar :: charList
            else
                char :: charList  -- todo : find a way to fuzz \t, \n, \r, so on
    ) ['"'] chars

escapeCharFuzz = 
    ['t', 'n', 'r', '"', '\'', '\\']
    |> List.map Fuzz.constant
    |> Fuzz.oneOf
    

suite : Test
suite =
    describe "Tokenizer"
        [ describe "tokenizeString"
            [ test "Empty string" <|
                \_ -> 
                    Tokenizer.tokenizeString ['"']
                    |> Expect.equal (Just ([], []))
                    
            , fuzz (tuple (list char, escapeCharFuzz)) "Legal string" <|
                \(chars, escapeChar) -> 
                    legalizeChars escapeChar chars
                    |> Tokenizer.tokenizeString
                    |> MoreMaybe.isJust
                    |> Expect.true "Expected the string tokenizer to succeed."
                
            , fuzz (tuple (list char, escapeCharFuzz)) "Non-terminated string" <|
                \(chars, escapeChar) ->
                    List.foldr (
                        \char charList ->
                        case char of 
                            '\\' -> '\\' :: escapeChar :: charList
                            '"' -> '\\' :: '"' :: charList
                            _ -> char :: charList
                    ) [] chars
                    |> Tokenizer.tokenizeString
                    |> MoreMaybe.isJust >> not
                    |> Expect.true "Fails when string is not terminated with brackets"
            ]
            
        , describe "tokenize"
            [ test "A general-case test string I wrote" <|
                \_ ->
                    String.toList "(module)"
                    |> Tokenizer.tokenize
                    |> Expect.equal (Just [LPar, StringLiteral <| String.toList "module", RPar])
            
            , test "Empty string" <|
                \_ ->
                    Tokenizer.tokenize []
                    |> Expect.equal (Just [])
            ]
        ]