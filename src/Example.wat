(module
    (func $add (param $lhs i32) (param $rhs i32) (result i32)
        local.get 0
        local.get 1
        i32.add
    )
    
    (func $empty) ;; It appears this is legal
    (func $justParams (param i32) (param i32))
    ;; (func $justResult (result i32)) ;; Fails at the typechecker phase


    (func $complex (param i32) (param i32)
        i32.const 0
        i32.const 1
        (i32.add (i32.const 0) (local.get 0))
    )
    
    (func $unfolded_loop
        loop $name
        nop
        end
    ) 
    
    (func $folded
        local.get 0
        local.get 1
        i32.add
        (i32.add (local.get 1) (local.get 2))
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
)