module Main exposing (..)

import Ast.Module as Module
import Browser
import File.Download as Download
import Html exposing (Html, div, text)
import Lexer
import Tokenizer


str = 
    """(module
   (func $folded
       (i32.add (i32.const 1) (i32.const 2))
   )
)"""
    |> String.toList


type alias Msg = ()


download txt = 
    Download.string "Output.txt" "text/plain" txt


init : () -> (String, Cmd Msg)
init _ = 
    Tokenizer.tokenize str
    |> (\x -> Debug.toString x |> Debug.log |> \_ -> x) 
    |> Maybe.andThen Lexer.lex
    |> Maybe.andThen Module.parse
    |> Maybe.map Module.toString
    |> Maybe.withDefault "ERROR"
    |> \x -> (x, download x)


view : String -> Html Msg
view model = div [] [text model]


update : Msg -> String -> (String, Cmd Msg)
update () string = (string, Cmd.none)
    

subscriptions : String -> Sub Msg
subscriptions _ = Sub.none


main = 
    Browser.element {init = init, view = view, update = update, subscriptions = subscriptions}