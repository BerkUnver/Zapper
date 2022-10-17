module Ast.ModuleTest exposing (..)

import Ast.Module as Module
import Expect
import Test exposing (Test, describe, test)

addFunc = 
    """(module
    (func $function (param $0 i32) (param $1 i32) (result i32)
        local.get $0
        local.get $1
        i32.add
    )
)""" 

factFunc =
    """(module
    (func $fact (param $n i32) (result i32)
        (if $unused (i32.lt_s (local.get $n) (i32.const 1))
            (then
                (return (i32.const 1))
            )
            (else
                (return (i32.mul (local.get $n) (call $fact (i32.sub (local.get $n) (i32.const 1)))))
            )
        )
    )
)"""

suite : Test
suite = 
    describe "Module" <|
        [ test "simple addition function" <|
            \_ ->
                Module.parse360 addFunc
                |> Expect.equal (Ok addFunc)
        , test "simple factorial function" <|
            \_ ->
            Module.parse360 factFunc
            |> Expect.equal (Ok factFunc)     
        ]