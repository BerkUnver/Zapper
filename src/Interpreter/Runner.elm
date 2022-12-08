module Interpreter.Runner exposing (..)

import Ast.Func exposing (Func)
import Ast.Instruction as Instruction exposing (ControlScope, FoldedInstr(..), IfScope, Instruction(..))
import Ast.Module exposing (Ast)
import Dict exposing (Dict)
import Interpreter.Frame as Frame exposing (Frame, RuntimeUpdate(..))
import Interpreter.Val as Val exposing (Val)
import More.Dict as Dict
import More.List as List
import ValType exposing (ValType)


and : Int -> Int -> Int
and lhs rhs = if lhs > 0 && rhs > 0 then 1 else 0


or : Int -> Int -> Int
or lhs rhs = if lhs > 0 || rhs > 0 then 1 else 0


xor : Int -> Int -> Int 
xor lhs rhs = if (rhs > 0 && lhs <= 0) || (rhs <= 0 && lhs > 0) then 1 else 0


eqz : number -> Int
eqz i = if i == 0 then 1 else 0


eq : number -> number -> Int
eq lhs rhs = if lhs == rhs then 1 else 0


ne : number -> number -> Int
ne lhs rhs = if lhs /= rhs then 1 else 0


lt : number -> number -> Int
lt lhs rhs = if lhs < rhs then 1 else 0


gt : number -> number -> Int
gt lhs rhs = if lhs > rhs then 1 else 0


le : number -> number -> Int
le lhs rhs = if lhs <= rhs then 1 else 0


ge : number -> number -> Int
ge lhs rhs = if lhs >= rhs then 1 else 0


runInstruction : Instruction -> Ast -> Frame -> Result String RuntimeUpdate
runInstruction instr ast frame =
    case instr of
        LocalGet label ->
            Frame.getLocal label frame
            |> Result.map (\val -> Frame.push val frame |> Updated)
        
        LocalSet label -> 
            Frame.pop frame
            |> Result.andThen (\(val, f) -> Frame.setLocal label val f |> Result.map Updated)
            
        LocalTee label -> 
            Frame.peek frame
            |> Result.andThen (\val -> Frame.setLocal label val frame |> Result.map Updated)
        
        Nop -> 
            Ok <| Updated frame
        
        Unreachable -> 
            Ok Trap
        
        Block block ->
            runScope block False ast frame
        
        Loop loop ->
            runScope loop True ast frame
    
        If ifScope ->
            runIf ifScope ast frame
        
        Br label ->
            (label, frame)
            |> Branch |> Ok
        
        BrIf label ->
            Frame.expectI32 frame
            |> Result.andThen (\(condition, newFrame) ->
                if (condition /= 0) then
                    (label, newFrame) 
                    |> Branch |> Ok
                else
                    newFrame
                    |> Updated |> Ok
            )
        
        Return ->
            Ok <| ReturnBranch frame.stack -- RuntimeUpdate.Return
            
        Call label ->
            case Dict.get label ast.functions of
                Just func ->
                    case List.tryTake (List.length func.params) frame.stack of
                        Just (params, restOfStack) ->
                            runFunction ast params func
                            |> Result.map (\r ->
                                case r of
                                    ReturnTrap ->
                                        Trap
                                    
                                    ReturnVoid -> 
                                        Updated {frame | stack = restOfStack}
                                    
                                    ReturnVal val ->
                                        Updated {frame | stack = val :: restOfStack}
                            )
                            
                        Nothing ->
                            "Tried to call function $" ++ label ++ " but there were not enough values on the stack."
                            |> Err
                Nothing -> 
                    "Tried to call nonexistent function $" ++ label ++ "."
                    |> Err
            
        I32Const i -> 
            Frame.push (Val.I32 i) frame 
            |> Updated |> Ok
        
        I64Const i ->
            Frame.push (Val.I64 i) frame 
            |> Updated |> Ok
            
        F32Const f -> 
            Frame.push (Val.F32 f) frame 
            |> Updated |> Ok
        
        F64Const f ->
            Frame.push (Val.F64 f) frame 
            |> Updated |> Ok
        
        F32Abs -> Frame.unOpF32 abs frame
        F64Abs -> Frame.unOpF64 abs frame
        F32Neg -> Frame.unOpF32 negate frame
        F64Neg -> Frame.unOpF64 negate frame
        F32Sqrt -> Frame.unOpF32 sqrt frame -- returns a NaN, same as wasm does
        F64Sqrt -> Frame.unOpF64 sqrt frame
        F32Ceil -> Frame.unOpF32 (ceiling >> toFloat) frame
        F64Ceil -> Frame.unOpF64 (ceiling >> toFloat) frame
        F32Floor -> Frame.unOpF32 (floor >> toFloat) frame
        F64Floor -> Frame.unOpF64 (floor >> toFloat) frame
        F32Trunc -> Frame.unOpF32 (truncate >> toFloat) frame
        F64Trunc -> Frame.unOpF64 (truncate >> toFloat) frame
        F32Nearest -> Frame.unOpF32 (round >> toFloat) frame
        F64Nearest -> Frame.unOpF64 (round >> toFloat) frame
        
        I32Add -> Frame.binOpI32 (+) frame
        I64Add -> Frame.binOpI64 (+) frame
        I32Sub -> Frame.binOpI32 (-) frame
        I64Sub -> Frame.binOpI64 (-) frame
        I32Mul -> Frame.binOpI32 (*) frame
        I64Mul -> Frame.binOpI64 (*) frame
        I32And -> Frame.binOpI32 and frame
        I64And -> Frame.binOpI64 and frame
        I32Or -> Frame.binOpI32 or frame
        I64Or -> Frame.binOpI64 or frame
        I32Xor -> Frame.binOpI32 xor frame
        I64Xor -> Frame.binOpI64 xor frame
        
        F32Add -> Frame.binOpF32 (+) frame
        F64Add -> Frame.binOpF64 (+) frame
        F32Sub -> Frame.binOpF32 (-) frame
        F64Sub -> Frame.binOpF64 (-) frame
        F32Mul -> Frame.binOpF32 (*) frame
        F64Mul -> Frame.binOpF64 (*) frame
        F32Div -> Frame.binOpF32 (/) frame
        F64Div -> Frame.binOpF64 (/) frame
        F32Min -> Frame.binOpF32 min frame
        F64Min -> Frame.binOpF64 min frame
        F32Max -> Frame.binOpF32 max frame
        F64Max -> Frame.binOpF64 max frame
        
        I32Eqz -> Frame.unOpI32 eqz frame
        I64Eqz -> Frame.unOpI64 eqz frame
        
        I32Eq -> Frame.binOpI32 eq frame
        I64Eq -> Frame.binOpI64 eq frame
        I32Ne -> Frame.binOpI32 ne frame
        I64Ne -> Frame.binOpI64 ne frame
        I32LtS -> Frame.binOpI32 lt frame
        I64LtS -> Frame.binOpI64 lt frame
        I32GtS -> Frame.binOpI32 gt frame
        I64GtS -> Frame.binOpI64 gt frame
        I32LeS -> Frame.binOpI32 le frame
        I64LeS -> Frame.binOpI64 le frame
        I32GeS -> Frame.binOpI32 ge frame
        I64GeS -> Frame.binOpI64 ge frame
        
        F32Eq -> Frame.relOpF32 eq frame
        F64Eq -> Frame.relOpF64 eq frame
        F32Ne -> Frame.relOpF32 ne frame
        F64Ne -> Frame.relOpF64 ne frame
        F32Lt -> Frame.relOpF32 lt frame
        F64Lt -> Frame.relOpF64 lt frame
        F32Gt -> Frame.relOpF32 gt frame
        F64Gt -> Frame.relOpF64 gt frame
        F32Le -> Frame.relOpF32 le frame
        F64Le -> Frame.relOpF64 le frame
        F32Ge -> Frame.relOpF32 ge frame
        F64Ge -> Frame.relOpF64 ge frame
        
        Folded folded ->
            runFolded folded ast frame


runScope : ControlScope -> Bool -> Ast -> Frame -> Result String RuntimeUpdate
runScope scope isLoop ast frame =
    {frame | stack = []}
    |> runInstructions scope.body ast
    |> Result.andThen (\blockResult ->
        case blockResult of
            Branch (label, newFrame) ->
                if Just label == scope.label then
                    if isLoop then
                        {frame | locals = newFrame.locals}
                        |> runScope scope True ast
                    else 
                        case (newFrame.stack, scope.result) of
                            (head :: _, Just result) ->
                                if Val.ofType result head then
                                    {newFrame | stack = head :: frame.stack}
                                    |> Updated |> Ok
                                else
                                    "value " 
                                    ++ Val.toString head 
                                    ++ " did not match expected result type " 
                                    ++ ValType.toString result 
                                    ++ " of the block $" ++ label  ++ "."
                                    |> Err
                                    
                            (_, Nothing) -> 
                                Ok <| Updated newFrame
                            
                            _ ->
                                "Stack did not match the expected result when the program branched to block $" 
                                ++ label ++ "."
                                |> Err
                else 
                    Ok blockResult
            
            Updated newFrame ->
                case (newFrame.stack, scope.result) of
                    ([head], Just result) ->
                        if Val.ofType result head then 
                            {newFrame | stack = head :: frame.stack}
                            |> Updated |> Ok
                        else
                            "Stack contained a " 
                            ++ Val.typeToString head
                            ++ " but the scope $" 
                            ++ Maybe.withDefault "unnamed" scope.label 
                            ++ " the program fell out of expected a "
                            ++ ValType.toString result ++ "."
                            |> Err
                            
                    ([], Nothing) ->
                        newFrame
                        |> Updated |> Ok
                    
                    _ -> 
                        "Stack did not match the expected result when the program fell out of scope $" 
                        ++ Maybe.withDefault "unnamed" scope.label ++ "."
                        |> Err
            
            x -> Ok x -- cascade traps or returns.
    )


-- todo : only place I rely on named record fields so I can pass both an IfScope and a FoldedIfScope into here.
runIf ifScope ast frame =
    Frame.expectI32 frame 
    |> Result.andThen (\(condition, newFrame) ->
        let 
            block : ControlScope
            block = 
                { label = ifScope.label
                , body = if condition /= 0 then ifScope.thenBlock else ifScope.elseBlock
                , result = ifScope.result
                }
        in
        runScope block False ast newFrame
    )

runFolded : FoldedInstr -> Ast -> Frame -> Result String RuntimeUpdate
runFolded folded ast frame =
    case folded of 
        FInstr (instr, nextFoldedInstructions) ->
            let 
                continue instructions fr =
                    case instructions of
                        [] -> 
                            Ok <| Updated fr
                        head :: tail ->
                            case runFolded head ast fr of
                                Ok (Updated newFrame) -> -- not good that this is essentially repeated 3x in this function body
                                    continue tail newFrame
                                x -> x
            in
            case continue nextFoldedInstructions frame of 
                Ok (Updated newFrame) -> 
                    runInstruction instr ast newFrame
                x -> x
                
        FLoop loop -> 
            runScope loop True ast frame
        
        FBlock block ->
            runScope block False ast frame
            
        FIf ifScope ->
            case ifScope.folded of 
                Nothing ->
                    runIf ifScope ast frame
                Just foldedIfInstructions ->
                    case runFolded foldedIfInstructions ast frame of
                        Ok (Updated newFrame) ->
                            runIf ifScope ast newFrame
                        x -> x
                    
               
runInstructions : List Instruction -> Ast -> Frame -> Result String RuntimeUpdate
runInstructions instructions ast frame =
    case instructions of 
        head :: tail ->
            case runInstruction head ast frame of
                Ok (Updated newFrame) ->
                    runInstructions tail ast newFrame
                x -> x
        
        [] -> 
            frame |> Updated |> Ok                
    
    
type FuncReturn
    = ReturnTrap
    | ReturnVal Val
    | ReturnVoid


--- Must pass in params backwards because that is the order they are chosen from the stack.
runFunction : Ast -> List Val -> Func -> Result String FuncReturn
runFunction ast params func =
    case -- get parameter, set default values
        Dict.allUnique func.params 
        |> Maybe.andThen (Dict.insertManyUnique func.locals) 
        |> Maybe.map (Dict.map (\_ t -> Val.default t)) 
    of 
        Nothing ->
            Err <| "Duplicate local or parameter names."
        Just locals ->
            let 
                revParams = List.reverse params
                
                setParams : List (String, ValType) -> List Val -> Frame -> Result String Frame
                setParams parameters values frame =
                    case (parameters, values) of
                        ((label, t) :: pTail, val :: vTail) ->
                            if Val.ofType t val then
                                {frame | locals = Dict.insert label val frame.locals}
                                |> setParams pTail vTail
                            else
                                "param $" ++ label ++ " is of type " ++ ValType.toString t ++ 
                                ", but the function was given a " ++ Val.typeToString val ++ "."
                                |> Err
                        
                        ([], []) ->
                            Ok frame
                        
                        _ ->
                            Err "Function got the wrong number of parameters"
            in
            { locals = locals, stack = []}
            |> setParams func.params revParams
            |> Result.andThen (\frame -> 
                runInstructions func.body ast frame
                |> Result.andThen (\update ->
                    case update of 
                        Updated finalFrame ->
                            case (finalFrame.stack, func.result) of
                                ([head], Just result) ->
                                    if Val.ofType result head then
                                        ReturnVal head
                                        |> Ok
                                    else
                                        "Function expected "
                                        ++ ValType.toString result 
                                        ++ ", but was given a " 
                                        ++ Val.typeToString head ++ "."
                                        |> Err
                                ([], Nothing) -> 
                                    Ok ReturnVoid
                                
                                _ ->
                                    Err "Stack does not match function's expected result."
                        
                        ReturnBranch finalStack ->
                            case (finalStack, func.result) of
                                (head :: _, Just result) ->
                                    if Val.ofType result head then
                                        Ok <| ReturnVal head
                                    else
                                        "Value on top of stack of type " 
                                        ++ Val.typeToString head 
                                        ++ " is not the result of the function " 
                                        ++ ValType.toString result 
                                        ++ " when the function explicitly returned."
                                        |> Err
                                (_, Nothing) ->
                                    Ok <| ReturnVoid
                                    
                                _ -> 
                                    Err "Stack does not expect function's result when explicit return is used."
                        Branch (label, _) ->
                            "Function tried to branch to nonexistent label $" ++ label ++ "."
                            |> Err
                        
                        Trap -> Ok ReturnTrap
                )
            )


runMain : Ast -> Result String FuncReturn
runMain ast =
    case Dict.get "main" ast.functions of
        Nothing -> Err "No main function found."
        Just main ->
            if main.params /= [] then
                Err "Main function must have no parameters."
            else
                runFunction ast [] main
