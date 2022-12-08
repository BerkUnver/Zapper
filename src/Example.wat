(module
    (func $add (param $lhs i32) (param $rhs i32) (result i32)
        local.get $lhs
        local.get $rhs
        i32.add
    )
    
    (type $addType (func (param i32) (param i32) (result i32)))
    (func $addTypeUse (type $addType)
        (i32.add (local.get 0) (local.get 1))
    )


    (func $foldedAdd (param i32) (param i32) (result i32)
        (i32.add (local.get 0) (local.get 1))
    )
    
    (func $unfolded_loop
        loop $name
        nop
        end
    )
    
    
    (func $fact (param $n i32) (result i32)
        (i32.lt_s (local.get $n) (i32.const 1))
        if
            (return (i32.const 1))
        end
        (return (i32.mul (local.get $n) (call $fact (i32.sub (local.get $n) (i32.const 1)))))
    )
    
    (func $factUnfolded (param $n i32) (result i32)
        local.get $n
        i32.const 1
        i32.lt_s
        if (result i32)
            i32.const 1
        else
            local.get $n
            i32.const 1
            i32.sub
            call $factUnfolded
            local.get $n
            i32.mul
        end
    )
    
    (func $ifTypeSignature (result i32)
        i32.const 0
        if $block (result i32)
            i32.const 2
        else
            i32.const 3
        end
    )
    
    (func $loopTypeSignature (result i32)
        loop $loop (result i32)
            i32.const +1
        end
    )
    
        
    (func $blockNameShadow
        block $a
            block $b
                block $a
                    br $a
                end
            end
        end        
    )
    
    (func $returnDrop (result i32) ;; return automatically drops unused values
        i32.const 0
        i32.const 0
        return
    )
)