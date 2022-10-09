module Ast.ModuleTest exposing (..)

import Ast.Module as Module
import Expect
import Lexer
import More.Maybe as Maybe
import Test exposing (describe, test)
import Tokenizer


str = 
    """
    (module
        (func (param $0 i32) (param $1 i32) (result i32)
            local.get $0
            local.get $1
            i32.add
        )
    )
    """ 
    |> String.toList

suite = 
    describe "Module" <|
        [ test "full" <|
            \_ ->
                let 
                    tokens = Tokenizer.tokenize str
                    lexerTokens = Maybe.andThen Lexer.lex tokens
                    ast = Maybe.andThen Module.parse lexerTokens
                    finalStr = Maybe.map Module.toString ast
                in
                Maybe.isJust finalStr
                |> Expect.true (
                    [Debug.toString tokens, Debug.toString lexerTokens, Debug.toString ast] 
                    |> List.map Debug.toString 
                    |> String.join "\n\n\n")
                
        ]