module Interpreter exposing (..)


import Ast.Module exposing (Ast)
import Dict exposing (Dict)
import More.Dict as Dict
import More.Maybe as Maybe
type Val
    = I32 Int
    | I64 Int
    | F32 Int
    | F64 Int
    

type OpOutput
    = BranchTo String
    | Trap
    | LabelErr String
    | TypeErr String
    | NewEnv FuncFrame


type alias FuncFrame = 
    { locals : Dict String Val
    , frames : List ScopeFrame
    }
    

type alias StackFrame = 
    { stack : List Val
    }
    
run : Ast -> OpOutput
run mod =
    let main = Dict.get "main" mod.functions in
    case main of
        Nothing -> LabelErr "No main function found."
        Just m ->
            if m.params /= [] then
                TypeErr "Main function must have no parameters."
            else
                let toLocal l = (l.label, l.dataType) in 
                case m.locals |> List.map (toLocal) |> Dict.allUnique of
                    Nothing -> LabelErr "Duplicate local names."
                    Just locals ->
                        let frame = {locals = locals, frame = }
                                    
                
    

    
