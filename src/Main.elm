module Main exposing (..)
import Ast.Module as Module
import Html
import Lexer
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

main =
    Tokenizer.tokenize str
    |> (\x -> Debug.toString x |> Debug.log |> \_ -> x) 
    |> Maybe.andThen Lexer.lex
    |> Maybe.andThen Module.parse
    |> Maybe.map Module.toString
    |> Maybe.withDefault "ERROR"
    |> Html.text