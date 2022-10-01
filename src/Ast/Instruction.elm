module Ast.Instruction exposing (..)
        
import Dict
import Lexer exposing (Token(..))
import SExpr exposing (SExpr(..))

type Instruction
    
    -- todo : indexing locals
    = LocalGet String
    | LocalSet String
    | LocalTee String
    -- todo : global instructions
    
    -- todo : vector, reference, parametric, variable, table, memory instructions
    
    | Nop
    | Unreachable
    | Block {name : String, body : List Instruction}
    | Loop {name : String, body : List Instruction}
    | If {thenBlock : List Instruction, elseBlock : List Instruction}
    -- todo : br, br_if, br_table, return, call, call_indirect
    
    | I32Const Int | I64Const Int | F32Const Float | F64Const Float
    | I32Add | I64Add | F32Add | F64Add
    | I32Sub | I64Sub | F32Sub | F64Sub
    | I32Mul | I64Mul | F32Mul | F64Mul
    | I32And | I64And | F32And | F64And
    | I32Or | I64Or | F32Or | F64Or 
    | I32Xor | I64Xor | F32Xor | F64Xor
    -- todo : clz, ctz, popcnt, div, rem_... instructions, skipping because idk how well this maps to Elm


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
        Atom (Instr "local.get") :: Atom (Var name) :: tail ->
            Just (LocalGet name, tail)
        
        Atom (Instr "local.set") :: Atom (Var name) :: tail ->
            Just (LocalSet name, tail)
        
        Atom (Instr "local.tee") :: Atom (Var name) :: tail ->
            Just (LocalTee name, tail)
        
        Atom (Instr "nop") :: tail ->
            Just (Nop, tail)
        
        Atom (Instr "unreachable") :: tail ->
            Just (Unreachable, tail)
        
        SExpr.List (Atom (Instr "block") :: Atom (Var name) :: blockBody) :: tail ->
            parseBody blockBody
            |> Maybe.map (\body -> (Block {name = name, body = body}, tail))
            
        SExpr.List (Atom (Instr "loop") :: Atom (Var name) :: loopBody) :: tail ->
            parseBody loopBody
            |> Maybe.map (\body -> (Loop {name = name, body = body}, tail))
        
        -- todo : if, loop + block + if unfolded
        Atom (Instr instr) :: tail ->
            let 
                operand =
                    case tail of
                        Atom a :: rest -> Just (a, rest)
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

        