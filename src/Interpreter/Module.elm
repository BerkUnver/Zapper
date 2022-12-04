module Interpreter.Module exposing (..)

import Ast.Func exposing (Func)
import Ast.Instruction exposing (Instruction(..))
import Ast.Module exposing (Ast)
import Dict exposing (Dict)
import Interpreter.Frame as Frame exposing (Frame, RuntimeErr(..))
import Interpreter.Val as Val exposing (Val)
import More.Dict as Dict
import More.List as List
import More.Maybe as Maybe
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


type RuntimeResult
    = RuntimeErr RuntimeErr
    | Branch (String, Frame)
    | Return Frame
    | Trap
    | Updated Frame


map : (Frame -> Frame) -> Result RuntimeErr Frame -> RuntimeResult
map func frame = 
    case frame of 
        Err e -> RuntimeErr e
        Ok f -> Updated <| func f



andThen : (Frame -> Result RuntimeErr Frame) -> Result RuntimeErr Frame -> RuntimeResult
andThen func frame =
    case frame of 
        Err e -> RuntimeErr e
        Ok f ->
            case func f of
                Err e -> RuntimeErr e
                Ok nf -> Updated nf


runInstruction : Instruction -> Ast -> Frame -> Result RuntimeErr Frame
runInstruction instr ast frame =
    case instr of
        LocalGet label ->
            Frame.getLocal label frame
            |> Result.map (\val -> Frame.push val frame)
        
        LocalSet label -> 
            Frame.pop frame
            |> Result.andThen (\(val, f) -> Frame.setLocal label val f)
            
        LocalTee label -> 
            Frame.peek frame
            |> Result.andThen (\val -> Frame.setLocal label val frame)
        
        Nop -> Ok frame
        
        Unreachable -> Err Trap
        
        Block block ->
            case runInstructions block.body ast {frame | stack = []} of
                Branch (label, newFrame) as br ->
                    if Just label /= block.label 
                        then br 
                    else
                        case (newFrame.stack, block.result) of
                            (head :: _, Just result) ->
                                if Val.ofType result head then
                                    Update{newFrame | stack = head :: frame.stack}
                                else
                                    "value " 
                                    ++ Val.toString head 
                                    ++ " did not match expected result type " 
                                    ++ ValType.toString result 
                                    ++ " of the block $" ++ label  ++ "."
                                    |> TypeErr
                                    |> RuntimeErr
                                    
                            (_, Nothing) -> 
                                Updated newFrame
                            
                            _ ->
                                "Stack did not match the expected result when the program branched to block $" 
                                ++ label ++ "."
                                |> TypeErr
                                |> RuntimeErr 
                               
                
                Updated newFrame ->
                    case (newFrame.stack, block.result) of
                        ([head], Just result) ->
                            if Val.ofType result head then 
                                Updated {newFrame | stack = head :: frame.stack}
                            else
                                
                                
                        ([], Nothing) ->
                            Updated newFrame
                        
                        _ -> 
                            "Stack did not match the expected result when the program fell out of block $" 
                            ++ block.label ++ "."
                            |> TypeErr
                            |> RuntimeErr
                    
                x -> x
        
        If if_ ->
            Frame.expectI32 frame
            |> Result.andThen(\(condition, newFrame) ->
                if condition == 0 then
                    
            )
        
        Call label ->
            case Dict.get label ast.functions of
                Nothing -> 
                    "Tried to call nonexistent function $" ++ label ++ "."
                    |> LabelErr |> Err
                
                Just func ->
                    case List.tryTake (List.length func.params) (Frame.getStack frame) of
                        Just params ->
                            runFunction ast params func
                            |> Result.map (Maybe.mapWithDefault frame (\r -> Frame.push r frame))
                            
                        Nothing ->
                           "Tried to call function $" ++ label ++ " but there were not enough values on the stack."
                           |> StackEmptyErr |> Err
            
        I32Const i -> 
            Frame.push (Val.I32 i) frame |> Ok
        
        I64Const i ->
            Frame.push (Val.I64 i) frame |> Ok
            
        F32Const f -> 
            Frame.push (Val.F32 f) frame |> Ok
        
        F64Const f ->
            Frame.push (Val.F64 f) frame |> Ok
            
        F32Abs -> Frame.unOpF32 abs frame
        F64Abs -> Frame.unOpF64 abs frame
        F32Neg -> Frame.unOpF32 negate frame
        F64Neg -> Frame.unOpF64 negate frame
        F32Sqrt -> Frame.unOpF32 sqrt frame -- todo : See what happens when the operand is negative
        F64Sqrt -> Frame.unOpF64 sqrt frame
        F32Ceil -> Frame.unOpF32 (ceiling >> toFloat) frame
        F64Ceil -> Frame.unOpF64 (ceiling >> toFloat) frame
        F32Floor -> Frame.unOpF32 (floor >> toFloat) frame
        F64Floor -> Frame.unOpF64 (floor >> toFloat) frame
        F32Trunc -> Frame.unOpF32 (truncate >> toFloat) frame
        F64Trunc -> Frame.unOpF64 (truncate >> toFloat) frame
        F32Nearest -> Frame.unOpF32 (round >> toFloat) frame
        F64Nearest -> Frame.unOpF64 (round >> toFloat) frame
        
        I32Add -> Frame.binOpI32 add frame
        I64Add -> Frame.binOpI64 add frame
        I32Sub -> Frame.binOpI32 sub frame
        I64Sub -> Frame.binOpI64 sub frame
        I32Mul -> Frame.binOpI32 mul frame
        I64Mul -> Frame.binOpI64 mul frame
        I32And -> Frame.binOpI32 and frame
        I64And -> Frame.binOpI64 and frame
        I32Or -> Frame.binOpI32 or frame
        I64Or -> Frame.binOpI64 or frame
        I32Xor -> Frame.binOpI32 xor frame
        I64Xor -> Frame.binOpI64 xor frame
        
        F32Add -> Frame.binOpF32 add frame
        F64Add -> Frame.binOpF64 add frame
        F32Sub -> Frame.binOpF32 sub frame
        F64Sub -> Frame.binOpF64 sub frame
        F32Mul -> Frame.binOpF32 mul frame
        F64Mul -> Frame.binOpF64 mul frame
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
        
        
        
                
runInstructions : List Instruction -> Ast -> Frame -> Result RuntimeErr Frame
runInstructions instructions ast frame =
    
        
runFunction : Ast -> List Val -> Func -> Result RuntimeErr (Maybe Val)
runFunction ast params func =
    case -- get parameter, set default values
        Dict.allUnique func.params 
        |> Maybe.andThen (Dict.insertManyUnique func.locals) 
        |> Maybe.map (Dict.map (\_ t ->
            case t of 
                ValType.I32 -> Val.I32 0
                ValType.I64 -> Val.I64 0
                ValType.F32 -> Val.F32 0
                ValType.F64 -> Val.F64 0
            )) 
    of 
        Nothing ->
            Err <| LabelErr <| "Duplicate labels."
        Just locals ->
            let 
                revParams = List.reverse params
                
                setParams : List (String, ValType) -> List Val -> FuncFrame -> Result RuntimeErr FuncFrame
                setParams parameters values frame =
                    case (parameters, values) of
                        ((label, t) :: pTail, val :: vTail) ->
                            if ofType t val then
                                {frame | locals = Dict.insert label val frame.locals}
                                |> setParams pTail vTail
                            else
                                "param $" ++ label ++ " is of type " ++ ValType.toString t ++ 
                                ", but the function was given a " ++ valToString val ++ "."
                                |> TypeErr |> Err
                        
                        ([], []) ->
                            Ok frame
                        
                        _ ->
                            "function got the wrong number of parameters" |> TypeErr |> Err
            in
            { locals = locals, stack = [], frames = []}
            |> setParams func.params revParams
            |> Result.andThen (runInstructions func.body)
                
        Nothing ->
            Err <| LabelErr "Duplicate local or parameter names."


run : Ast -> Result RuntimeErr (Maybe Val)
run mod =
    case Dict.get "main" mod.functions of
        Nothing -> Err <| LabelErr "No main function found."
        Just main ->
            if main.params /= [] then
                Err <| TypeErr "Main function must have no parameters."
            else
                case Dict.allUnique main.locals of
                    Nothing -> Err <| LabelErr "Duplicate local names."
                    Just locals ->
                        let frame =  in
                        runInstructions frame [] main