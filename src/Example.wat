(module
    (export "add" (func $add))
    (func $add (param $lhs i32) (param $rhs i32) (result i32)
        local.get 0
        local.get 1
        i32.add
    )
    
    (func $empty) ;; It appears this is legal
    (func $justParams (param i32) (param i32))
    ;; (func $justResult (result i32)) ;; Fails at the typechecker phase


    (func $complex (param i32) (param i32) ;; Idk what's happening here
        i32.add 
        (local.get 1)
        (local.get 0)
    )
)