module Main exposing (..)

import Ast.Module as Module
import Browser
import File.Download as Download
import Html exposing (Html, div, text)
import More.Result as Result


str = 
    """(module
   (func $add (param $lhs i32) (param $rhs i32) (result i32)
       local.get $lhs
       local.get $rhs
       i32.add
   )
   
   (func $empty)
   
   (func $justParams (param $0 i32) (param $1 i32))
   
   (func $unfoldedLoop
       loop $name
       nop
       end
   )
   
   (func $folded (result i32)
       (i32.add (i32.const 1) (i32.const 2))
   )
   
   (func $fact (param $n i32) (result i32)
       (if (i32.lt_s (local.get $n) (i32.const 1))
           (then 
               (return (i32.const 1)) 
           )
           (else 
               (return (i32.mul (local.get $n) (call $fact (i32.sub (local.get $n) (i32.const 1)))))
           )
       )
   )
)"""


type alias Msg = ()


download txt = 
    Download.string "Output.txt" "text/plain" txt


init : () -> (String, Cmd Msg)
init _ =
    Module.parse360 str
    |> Result.unwrap
    |> \x -> (x, download x)


view : String -> Html Msg
view model = div [] [text model]


update : Msg -> String -> (String, Cmd Msg)
update () string = (string, Cmd.none)
    

subscriptions : String -> Sub Msg
subscriptions _ = Sub.none


main = 
    Browser.element {init = init, view = view, update = update, subscriptions = subscriptions}