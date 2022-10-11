module Ast.Instruction exposing (..)
        
import Dict
import Format
import Lexer exposing (Token(..))
import More.List as List
import More.String as String


type FoldedInstr
    = FInstr (Instruction, List FoldedInstr)
    | FIf {label : String, folded : List FoldedInstr,  thenBlock : List Instruction, elseBlock : List Instruction}
    | FLoop {label : String, body : List Instruction}
    | FBlock {label : String, body : List Instruction}
    
    
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
    | Br String
    | BrIf String
    | Return
    | Call String
    -- todo : br_table, call_indirect
    
    | I32Const Int | I64Const Int 
    | F32Const Float | F64Const Float
    
    -- todo : clz, ctz, popcnt
    
    | F32Abs | F64Abs
    | F32Neg | F64Neg
    | F32Sqrt | F64Sqrt
    | F32Ceil | F64Ceil
    | F32Floor | F64Floor
    | F32Trunc | F64Trunc
    | F32Nearest | F64Nearest
    
    | I32Add | I64Add 
    | I32Sub | I64Sub 
    | I32Mul | I64Mul 
    | I32And | I64And 
    | I32Or  | I64Or 
    | I32Xor | I64Xor
    
    | F32Add | F64Add
    | F32Sub | F64Sub
    | F32Mul | F64Mul
    | F32Min | F64Min
    | F32Max | F64Max
    -- todo : copysign
    
    | I32Eqz | I64Eqz
    
    | I32Eq | I64Eq
    | I32Ne | I64Ne
    | I32LtS | I64LtS
    | I32GtS | I64GtS
    | I32LeS | I64LeS
    | I32GeS | I64GeS
    -- todo : lt_u, gt_u, le_u, ge_u (unsigned less than)
    
    | F32Eq | F64Eq
    | F32Ne | F64Ne
    | F32Lt | F64Lt
    | F32Gt | F64Gt
    | F32Le | F64Le
    | F32Ge | F64Ge
    
    | Folded FoldedInstr
    -- todo : , div, rem_... instructions, skipping because idk how well this maps to Elm


indentInstructions i = 
    List.map (toString >> Format.indent) i
    |> String.joinWithFirst "\n"


foldedToString : FoldedInstr -> String
foldedToString instr = 
    case instr of
        FInstr (head, tail) ->
            "(" ++ toString head ++ String.joinWithFirst " " (List.map foldedToString tail) ++ ")"
            
        FIf ifInstr ->
            let 
                if_ = "(if $" ++ ifInstr.label ++ String.joinWithFirst " " (List.map foldedToString ifInstr.folded) ++ "\n"
                then_ = Format.indent <| "(then" ++ indentInstructions ifInstr.thenBlock ++ "\n)"
                else_ = Format.indent <| "(else" ++ indentInstructions ifInstr.elseBlock ++ "\n)"
            in
            if_ ++ then_ ++ "\n" ++ else_ ++ "\n)"
            
        FLoop loop ->
            "(loop $" ++ loop.label ++ "\n" ++ indentInstructions loop.body ++ "\n)"
        
        FBlock block ->
            "(block $" ++ block.label ++ indentInstructions block.body ++ "\n)"

    
toString : Instruction -> String
toString instr =
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
            
        Loop loop ->
            "loop $" ++ loop.label 
            ++ indentInstructions loop.body 
            ++ "\nend"
            
        Block block ->
            "block $" ++ block.label 
            ++ indentInstructions block.body 
            ++ "\nend"
       
        If if_ ->
            "if $" ++ if_.label 
            ++ indentInstructions if_.thenBlock 
            ++ "\nelse"
            ++ indentInstructions if_.elseBlock
            ++ "\nend"
        
        Br label ->
            "br $" ++ label
        
        BrIf label ->
            "br_if $" ++ label
            
        Return ->
            "return"
                    
        Call name ->
            "call $" ++ name
            
        I32Const i -> 
            "i32.const " ++ String.fromInt i
        
        I64Const i ->
            "i64.const " ++ String.fromInt i
            
        F32Const i -> 
            "f32.const " ++ String.fromFloat i
        
        F64Const i ->
            "f64.const " ++ String.fromFloat i
        
        Folded f ->
            foldedToString f
             
        _ ->
            singleNumericOps
            |> List.firstJust (\(str, op) -> if op == instr then Just str else Nothing)
            |> Maybe.withDefault "unknown_instruction"
        
            
singleNumericOps =
    [ ("f32.abs", F32Abs), ("f64.abs", F64Abs) 
    , ("f32.neg", F32Neg), ("f64.neg", F64Neg)
    , ("f32.sqrt", F32Sqrt), ("f64.sqrt", F64Sqrt)
    , ("f32.ceil", F32Ceil), ("f64.ceil", F64Ceil)
    , ("f32.floor", F32Floor), ("f64.floor", F64Floor)
    , ("f32.trunc", F32Trunc), ("f64.trunc", F64Trunc)
    , ("f32.nearest", F32Nearest), ("f64.nearest", F64Nearest)
    
    , ("i32.add", I32Add), ("i64.add", I64Add)
    , ("i32.sub", I32Sub), ("i64.sub", I64Sub)
    , ("i32.mul", I32Mul), ("i64.mul", I64Mul)
    , ("i32.and", I32And), ("i64.and", I64And)
    , ("i32.or", I32Or), ("i64.or", I64Or)
    , ("i32.xor", I32Xor), ("i64.xor", I64Xor)
    
    , ("f32.add", F32Add), ("f64.add", F64Add)
    , ("f32.sub", F32Sub), ("f64.sub", F64Sub)
    , ("f32.mul", F32Mul), ("f64.mul", F64Mul)
    , ("f32.min", F32Min), ("f64.min", F64Min)
    , ("f32.max", F32Max), ("f64.max", F64Max)
    
    , ("i32.eqz", I32Eqz), ("i64.eqz", I64Eqz)
    
    , ("i32.eq", I32Eq), ("i64.eq", I64Eq)
    , ("i32.ne", I32Ne), ("i64.ne", I64Ne)
    , ("i32.lt_s", I32LtS), ("i64.lt_s", I64LtS)
    , ("i32.gt_s", I32GtS), ("i64.gt_s", I64GtS)
    , ("i32.le_s", I32LeS), ("i64.le_s", I64LeS)
    , ("i32.ge_s", I32GeS), ("i64.ge_s", I64GeS)
    
    , ("f32.eq", F32Eq), ("f64.eq", F64Eq)
    , ("f32.ne", F32Ne), ("f64.ne", F64Ne)
    , ("f32.lt", F32Lt), ("f64.lt", F64Lt)
    , ("f32.gt", F32Gt), ("f64.gt", F64Gt)
    , ("f32.le", F32Le), ("f64.le", F64Le)
    , ("f32.ge", F32Ge), ("f64.ge", F64Ge)
    ]



stringToSingleNumeric = Dict.fromList singleNumericOps


parseFolded : List Token -> Maybe FoldedInstr
parseFolded tokens = 
    let
        parseFoldedNormal t = 
            case t of 
                Scope folded ->
                    parseFolded folded
                _ -> Nothing
    in
    case tokens of 
        Instr "block" :: Var name :: blockBody ->
            parse blockBody
            |> Maybe.map (\body -> FBlock {label = name, body = body})
            
        Instr "loop" :: Var name :: loopBody ->
            parse loopBody
            |> Maybe.map (\body -> FLoop {label = name, body = body})
            
        Instr "if" ::  Var name :: ifBody ->
            let 
                parseIf body = 
                    case body of 
                        [Scope (Instr "then" :: thenBody), Scope (Instr "else" :: elseBody)] ->
                            parse thenBody
                            |> Maybe.andThen (\thenB ->
                                parse elseBody
                                |> Maybe.map (\elseB -> {label = name, folded = [], thenBlock = thenB, elseBlock = elseB}))
                                
                        Scope instructions :: tail ->
                            parseFolded instructions
                            |> Maybe.andThen (\folded -> 
                                parseIf tail 
                                |> Maybe.map (\x -> {x | folded = folded :: x.folded}))
                        
                        _ -> Nothing
            in
            parseIf ifBody
            |> Maybe.map FIf
        
        _ -> 
            parseSingle tokens
           |> Maybe.andThen (\(folded, tail) -> 
               List.tryAll parseFoldedNormal tail
               |> Maybe.map (\x -> FInstr (folded, x)))
    
    
parseSingle tokens =
    case tokens of 
        Instr "local.get" :: Var name :: tail ->
            Just (LocalGet name, tail)
        
        Instr "local.set" :: Var name :: tail ->
            Just (LocalSet name, tail)
        
        Instr "local.tee" :: Var name :: tail ->
            Just (LocalTee name, tail)
            
        Instr "block" :: Var name :: tail ->
            parseToEndToken tail
            |> Maybe.map (\(block, rest) -> (Block {label = name, body = block}, rest))
            
        Instr "loop" :: Var name :: tail ->
            parseToEndToken tail
            |> Maybe.map (\(loop, rest) -> (Loop {label = name, body = loop}, rest))   
            
        Instr "if" :: Var name :: tail -> 
            parseToSymbol "else" tail
            |> Maybe.andThen (\(thenB, elseTokens) ->
                parseToEndToken elseTokens
                |> Maybe.map (\(elseB, rest) -> (If {label = name, thenBlock = thenB, elseBlock = elseB}, rest)))                 
        
        Instr "br" :: Var name :: tail ->
            Just (Br name, tail)
            
        Instr "br_if" :: Var name :: tail ->
            Just (BrIf name, tail)
        
        Instr "nop" :: tail ->
            Just (Nop, tail)
        
        Instr "unreachable" :: tail ->
            Just (Unreachable, tail)
        
        Instr "return" :: tail ->
            Just (Return, tail)
        
        Instr "call" :: Var name :: tail ->
            Just (Call name, tail)
                       
        
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
        
        Scope instructions :: tail ->
            parseFolded instructions
            |> Maybe.map (\i -> (Folded i, tail))
        
        _ -> Debug.todo "rest of instructions"


parseToSymbol name tokens = 
    let 
        parseNormally () = 
           parseSingle tokens
           |> Maybe.andThen (\(instr, tail) -> 
               parseToEndToken tail |> Maybe.map (\(instructions, unparsed) -> 
                   (instr :: instructions, unparsed))) 
    in
    case tokens of 
        [] -> Nothing
        Instr instr :: tail ->
            if instr == name then
                Just ([], tail)
            else 
                parseNormally ()
        _ ->
            parseNormally ()
 
 
parseToEndToken tokens = 
    parseToSymbol "end" tokens

                
parse tokens =
    case tokens of
        [] -> 
            Just []
        _ -> 
            parseSingle tokens
            |> Maybe.andThen (\(i, rest) -> parse rest |> Maybe.map (\instructions -> i :: instructions))

        