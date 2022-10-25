﻿module Typechecker.Module exposing (..)

import Ast.Func exposing (Func)
import Ast.Instruction exposing (Instruction(..))
import Dict exposing (Dict)
import More.List as List
import ValType exposing (ValType)


type alias ModuleEnv = 
    { functions : Dict String Func}
    

type alias StackFrame = 
    { label : Maybe String
    , stack : List ValType
    , unreachable : Bool
    }


emptyFrame : StackFrame
emptyFrame = 
    { label = Nothing
    , stack = []
    , unreachable = False
    }


type alias FuncEnv = 
    { locals : Dict String ValType
    , stackFrame : StackFrame
    , oldStackFrames : List StackFrame
    , unreachable : Bool
    }


newFuncEnv : Dict String ValType -> FuncEnv
newFuncEnv locals = 
    { locals = locals
    , stackFrame = emptyFrame
    , oldStackFrames = []
    , unreachable = False
    }


localDNE : String -> String
localDNE name = 
    "The local $" ++ name ++ " does not exist in the body of the current function."


pop : List ValType -> List ValType -> Maybe (List ValType)
pop paramTypes stack =
    case List.popAllEq paramTypes stack of
        ([], poppedStack) -> 
            Just poppedStack
        _ -> 
            Nothing


type TypeCheckResult
    = TypeOk FuncEnv
    | TypeReturn FuncEnv
    | TypeErr String
    | TypeBr (List String)


-- checkInstructions2 : ModuleEnv -> FuncEnv -> List Instruction -> TypeCheckResult
-- checkInstructions2 = Debug.todo "a mockup so I can figure out how loops work"


checkInstructions2 : ModuleEnv -> FuncEnv -> List Instruction -> TypeCheckResult
checkInstructions2 moduleEnv funcEnv instructions = 
    case instructions of
        instruction :: tail ->
            case instruction of
                Br label ->
                    case checkInstructions moduleEnv {funcEnv | unreachable = True} tail of
                        TypeOk [] -> instruction
                        TypeReturn valTypes -> 
                        TypeErr err ->
                    TypeBr [label]
                
                Loop {label, body} ->
                    case checkI 
            























resultToTypeCheckResult result = 
    case result of 
        Ok x -> TypeOk x
        Err x -> TypeErr x


checkInstructions : ModuleEnv -> FuncEnv -> List Instruction -> TypeCheckResult
checkInstructions moduleEnv funcEnv instructions = 
    case instructions of
        head :: tail ->
            case checkInstr moduleEnv funcEnv head of
                TypeOk stack ->
                    checkInstructions moduleEnv {funcEnv | stack = stack} tail
                
                x -> x
        
        []  -> TypeOk funcEnv.stack


checkBlock : ModuleEnv -> Dict String ValType -> String -> List Instruction -> TypeCheckResult
checkBlock moduleEnv locals label instructions =
    case checkInstructions moduleEnv {locals = locals, stack = []} instructions of
        TypeBr branchLabel as br ->
            if branchLabel == label then TypeOk [] else br
        ret -> ret


checkFunc : Func -> ModuleEnv -> Maybe String 
checkFunc func moduleEnv = 
    let 
        params =
            func.params
            |> List.map (\param -> (param.label, param.dataType))
            |> Dict.fromList
        
        funcEnv = {locals = params, stack = []}
    in 
    case checkInstructions moduleEnv funcEnv func.body of
        TypeOk stack ->
            if func.results == stack then
                Nothing
            else 
                "The function $" ++ func.label
                ++ " exited the type checker with a stack of "
                ++ Debug.toString stack
                ++ ", but the function is expecting a resulting stack of "
                ++ Debug.toString func.results
                |> Just
        
        TypeReturn stack ->
            if func.results == stack then
                Nothing
            else 
                "The function $" ++ func.label 
                ++ " tried to use the return instruction to return a stack of " 
                ++ Debug.toString stack
                ++ " when the function is expecting "
                ++ Debug.toString func.results
                |> Just  
        
        TypeBr label ->
            "The function $" ++ func.label
            ++ " tried to branch to the nonexistent label $"
            ++ label
            |> Just
        
        TypeErr err -> Just err


checkInstr : ModuleEnv -> FuncEnv -> Instruction -> TypeCheckResult
checkInstr moduleEnv funcEnv instr = 
    case instr of 
        LocalGet name ->
            case Dict.get name funcEnv.locals of
                Just valType -> 
                    TypeOk <| valType :: funcEnv.stack
                Nothing ->
                    TypeErr <| localDNE name
         
        LocalSet name ->
            case Dict.get name funcEnv.locals of
                Just t ->
                    case funcEnv.stack of
                        head :: tail -> 
                            if head == t then 
                                TypeOk tail
                            else
                                TypeErr <| "local.set : The type " ++ ValType.toString head ++ " on the stack does not match " ++ ValType.toString t ++ " of the local $" ++ name
                        [] -> 
                            TypeErr <| "Cannot local.set $" ++ name ++ " because the stack is empty."
               
                Nothing ->
                    TypeErr <| localDNE name 
        
        LocalTee name ->
            case Dict.get name funcEnv.locals of
                Just t ->
                    case funcEnv.stack of 
                        head :: tail ->
                            if head == t then
                                TypeOk <| t :: tail
                            else
                                TypeErr <| "local.tee : "
        
        Nop -> 
            TypeOk funcEnv.stack
        
        Unreachable -> 
            TypeOk funcEnv.stack
                    
        Block {label, body} ->
            checkBlock moduleEnv funcEnv.locals label body
                
        Loop {label, body} ->
            checkBlock moduleEnv funcEnv.locals label body
            
        If {label, thenBlock, elseBlock} ->
            let 
                env = {funcEnv | stack = []}
                then_ = checkInstructions moduleEnv env thenBlock
                else_ = checkInstructions moduleEnv env elseBlock
            in
            case (then_, else_) of
                
        
        Br label -> 
            TypeBr label
        
        BrIf label -> 
            TypeBr label
        
        Return -> 
            TypeReturn funcEnv.stack
        
        Call label ->
            Dict.get label moduleEnv.functions
            |> Result.fromMaybe ("Tried to call nonexistent function $" ++ label)
            |> Result.andThen (\fn -> 
                popThenPush (List.map .dataType fn.params) fn.results funcEnv.stack 
                |> Result.fromMaybe ("The parameters of called function $" ++ fn.label ++ " don't match what is on the stack."))
            |> resultToTypeCheckResult
            
        -- todo : if, br, br_if,         
