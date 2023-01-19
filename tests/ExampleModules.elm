module ExampleModules exposing (..)

factorial = """
(module
    (func $fact (param $n i32) (result i32)
        (i32.lt_s (local.get $n) (i32.const 1))
        if
            (return (i32.const 1))
        end
        (return (i32.mul (local.get $n) (call $fact (i32.sub (local.get $n) (i32.const 1)))))
    )
)
"""