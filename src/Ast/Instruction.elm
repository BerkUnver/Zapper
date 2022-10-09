module Ast.Instruction exposing (..)
        
import Dict
import Format
import Lexer exposing (Token(..))
import More.List as List


type Instruction
    
    -- todo : indexing locals
    = LocalGet String
    | LocalSet String
    | LocalTee String
    -- todo : global instructions
    
    -- todo : vector, reference, parametric, variable, table, memory instructions
    
    | Nop
    | Unreachable
    | Block {label : String, body : List Instruction}
    | Loop {label : String, body : List Instruction}
    | If {label : String, thenBlock : List Instruction, elseBlock : List Instruction}
    -- todo : br, br_if, br_table, return, call, call_indirect
    
    | I32Const Int | I64Const Int | F32Const Float | F64Const Float
    | I32Add | I64Add | F32Add | F64Add
    | I32Sub | I64Sub | F32Sub | F64Sub
    | I32Mul | I64Mul | F32Mul | F64Mul
    | I32And | I64And | F32And | F64And
    | I32Or | I64Or | F32Or | F64Or 
    | I32Xor | I64Xor | F32Xor | F64Xor
    
    -- todo : clz, ctz, popcnt, div, rem_... instructions, skipping because idk how well this maps to Elm


toString : Instruction -> String
toString instr =
    let indentInstructions i = i |> List.map toString |> String.join "\n" |> Format.indent in
    case instr of
        LocalGet str -> 
            "local.get $" ++ str
            
        LocalSet str -> 
            "local.set $" ++ str
            
        LocalTee str -> 
            "local.tee $" ++ str 
            
        Nop ->
            "nop"
        
        Unreachable -> 
            "unreachable"
        
        Block block -> 
            "(block $" ++ block.label ++ "\n" ++ indentInstructions block.body ++ "\n)"
        
        Loop loop -> 
            "(loop $" ++ loop.label ++ "\n" ++ indentInstructions loop.body ++ "\n)"
            
        If ifInstr -> 
            let 
                if_ = "(if $" ++ ifInstr.label ++ "\n"
                then_ = Format.indent <| "(then\n" ++ indentInstructions ifInstr.thenBlock ++ "\n)"
                else_ = Format.indent <| "(else\n" ++ indentInstructions ifInstr.elseBlock ++ "\n)"
                end = ")"
            in
            if_ ++ then_ ++ "\n" ++ else_ ++ "\n)"
        
        I32Const i -> 
            "i32.const" ++ String.fromInt i
        
        I64Const i ->
            "i64.const" ++ String.fromInt i
            
        F32Const i -> 
            "f32.const" ++ String.fromFloat i
        
        F64Const i ->
            "f64.const" ++ String.fromFloat i
            
        _ ->
            singleNumericOps
            |> List.firstJust (\(str, op) -> if op == instr then Just str else Nothing)
            |> Maybe.withDefault "unknown_instruction"
        
            
singleNumericOps = 
    [ ("i32.add", I32Add)
    , ("i64.add", I64Add)
    , ("f32.add", F32Add)
    , ("f64.add", F64Add)
    , ("i32.sub", I32Sub)
    , ("i64.sub", I64Sub)
    , ("f32.sub", F32Sub)
    , ("f64.sub", F64Sub)
    , ("i32.mul", I32Mul)
    , ("i64.mul", I64Mul)
    , ("f32.mul", F32Mul)
    , ("f64.mul", F64Mul)
    , ("i32.and", I32And)
    , ("i64.and", I64And)
    , ("f32.and", F32And)
    , ("f64.and", F64And)
    , ("i32.or", I32Or)
    , ("i64.or", I64Or)
    , ("f32.or", F32Or)
    , ("f64.or", F64Or)
    , ("i32.xor", I32Xor)
    , ("i64.xor", I64Xor)
    , ("f32.xor", F32Xor)
    , ("f64.xor", F64Xor)
    ]



stringToSingleNumeric = Dict.fromList singleNumericOps


parseSingle tokens = 
    case tokens of 
        Instr "local.get" :: Var name :: tail ->
            Just (LocalGet name, tail)
        
        Instr "local.set" :: Var name :: tail ->
            Just (LocalSet name, tail)
        
        Instr "local.tee" :: Var name :: tail ->
            Just (LocalTee name, tail)
        
        Instr "nop" :: tail ->
            Just (Nop, tail)
        
        Instr "unreachable" :: tail ->
            Just (Unreachable, tail)
        
        Scope (Instr "block" :: Var name :: blockBody) :: tail ->
            parseBody blockBody
            |> Maybe.map (\body -> (Block {label = name, body = body}, tail))
            
        Scope (Instr "loop" :: Var name :: loopBody) :: tail ->
            parseBody loopBody
            |> Maybe.map (\body -> (Loop {label = name, body = body}, tail))
        
        -- todo : loop + block + if unfolded
        
        Scope [Instr "if", Var name, Scope (Instr "then" :: ifBody), Scope (Instr "else" :: elseBody)] :: tail ->
            parseBody ifBody
            |> Maybe.andThen (\thenB ->
            parseBody elseBody
            |> Maybe.map 
            (\elseB -> (If {label = name, thenBlock = thenB, elseBlock = elseB}, tail))) 
            
        -- todo : Normal folded                        
        Instr instr :: tail ->
            let 
                operand =
                    case tail of
                        head :: rest -> Just (head, rest)
                        _ -> Nothing
                                         
                ifRandInt func = 
                    case operand of
                        Just (UInt i, rest) -> Just <| (func i, rest)
                        Just (Int i, rest) -> Just <| (func i, rest)
                        _ -> Nothing
                
                ifRandFloat func =
                    case operand of
                        Just (Float f, rest) -> Just <| (func f, rest)
                        _ -> Nothing
            in 
            case instr of
                "i32.const" -> ifRandInt I32Const  
                "i64.const" -> ifRandInt I64Const
                "f32.const" -> ifRandFloat F32Const
                "f64.const" -> ifRandFloat F64Const
                _ ->
                    Dict.get instr stringToSingleNumeric
                    |> Maybe.map (\i -> (i, tail))
                
        _ -> Debug.todo "rest of instructions"
    
parseBody tokens =
    case tokens of
        [] -> 
            Just []
        _ -> 
            parseSingle tokens
            |> Maybe.andThen (\(i, rest) ->
            parseBody rest |> Maybe.map (\instructions ->
            i :: instructions))

        