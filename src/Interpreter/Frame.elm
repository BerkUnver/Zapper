module Interpreter.Frame exposing (..)

import Dict exposing (Dict)
import Interpreter.Val as Val exposing (Val)


type alias Frame =
    { locals : Dict String Val
    , stack : List Val
    }


type RuntimeUpdate
    = Branch (String, Frame)
    | ReturnBranch (List Val) -- todo : better name
    | Trap
    | Updated Frame


getLocal : String -> Frame -> Result String Val
getLocal label frame =
    Dict.get label frame.locals
    |> Result.fromMaybe ("Local with label $" ++ label ++ " does not exist.")


setLocal : String -> Val -> Frame -> Result String Frame
setLocal label val frame =
    getLocal label frame
    |> Result.andThen (\local -> 
        if Val.sameType val local then
            Ok {frame | locals = Dict.insert label val frame.locals}
        else
            "Expected local to be of type " 
            ++ Val.typeToString val 
            ++ " but it was of type " 
            ++ Val.typeToString local
            |> Err
    )


peek : Frame -> Result String Val
peek frame = 
    case frame.stack of
        [] -> Err "Tried to peek from an empty stack."
        head :: _ -> Ok head


pop : Frame -> Result String (Val, Frame)
pop frame = 
    case frame.stack of
        [] -> 
            Err "Tried to pop from an empty stack."
        head :: tail -> 
            Ok (head, {frame | stack = tail})


push : Val -> Frame -> Frame
push val frame = {frame | stack = val :: frame.stack}


-- todo : Find a way to get rid of this horribleness

expectI32 frame =
    pop frame
    |> Result.andThen (\(x, fr) -> 
        case x of
            Val.I32 i -> Ok (i, fr)
            t -> Err <| "expected i32, got " ++ Val.typeToString t ++ "."
    )
        

expectI64 frame = 
    pop frame
    |> Result.andThen (\(x, fr) ->
        case x of
            Val.I64 i -> Ok (i, fr)
            t -> Err <| "expected i64, got " ++ Val.typeToString t ++ "."
        )


expectF32 frame = 
    pop frame
    |> Result.andThen (\(x, fr) ->
        case x of
            Val.F32 f -> Ok (f, fr)
            t -> Err <| "expected f32, got " ++ Val.typeToString t ++ "."
        )


expectF64 frame = 
    pop frame
    |> Result.andThen (\(x, fr) ->
        case x of
            Val.F64 f -> Ok (f, fr)
            t -> Err <| "expected f64, got " ++ Val.typeToString t ++ "."
        )


unOpI32 : (Int -> Int) -> Frame -> Result String RuntimeUpdate
unOpI32 op frame = 
    expectI32 frame
    |> Result.map (\(i32, fr) -> push (Val.I32 <| op i32) fr |> Updated)


unOpI64 : (Int -> Int) -> Frame -> Result String RuntimeUpdate
unOpI64 op frame = 
    expectI64 frame
    |> Result.map (\(i64, fr) -> push (Val.I64 <| op i64) fr |> Updated)


unOpF32 : (Float -> Float) -> Frame -> Result String RuntimeUpdate
unOpF32 op frame = 
    expectF32 frame
    |> Result.map (\(f32, fr) -> push (Val.F32 <| op f32) fr |> Updated)


unOpF64 : (Float -> Float) -> Frame -> Result String RuntimeUpdate
unOpF64 op frame =
    expectF64 frame
    |> Result.map (\(f64, newFrame) -> push (Val.F64 <| op f64) newFrame |> Updated)
    

binOpI32 : (Int -> Int -> Int) -> Frame -> Result String RuntimeUpdate
binOpI32 op frame =
    expectI32 frame
    |> Result.andThen (\(rhs, f1) ->
        expectI32 f1
        |> Result.map (\(lhs, fr) ->
            push (op lhs rhs |> Val.I32) fr
            |> Updated))
        

binOpI64 : (Int -> Int -> Int) -> Frame -> Result String RuntimeUpdate
binOpI64 op frame =
    expectI64 frame
    |> Result.andThen (\(rhs, f1) ->
        expectI64 f1
        |> Result.map (\(lhs, fr) ->
            push (op lhs rhs |> Val.I64) fr
            |> Updated))
        
        
binOpF32 : (Float -> Float -> Float) -> Frame -> Result String RuntimeUpdate
binOpF32 op frame =
    expectF32 frame
    |> Result.andThen (\(rhs, f1) ->
        expectF32 f1
        |> Result.map (\(lhs, fr) ->
            push (op lhs rhs |> Val.F32) fr
            |> Updated))
        
        
binOpF64 : (Float -> Float -> Float) -> Frame -> Result String RuntimeUpdate
binOpF64 op frame =
    expectF64 frame
    |> Result.andThen (\(rhs, f1) ->
        expectF64 f1
        |> Result.map (\(lhs, fr) ->
            push (op lhs rhs |> Val.F64) fr
            |> Updated))
        
        
relOpF32 : (Float -> Float -> Int) -> Frame -> Result String RuntimeUpdate
relOpF32 op frame =
    expectF32 frame
    |> Result.andThen (\(rhs, f1) ->
        expectF32 f1
        |> Result.map (\(lhs, fr) ->
            push (op lhs rhs |> Val.I32) fr
            |> Updated))
        
        
relOpF64 : (Float -> Float -> Int) -> Frame -> Result String RuntimeUpdate
relOpF64 op frame =
    expectF64 frame
    |> Result.andThen (\(rhs, f1) ->
        expectF64 f1
        |> Result.map (\(lhs, fr) ->
            push (op lhs rhs |> Val.I32) fr
            |> Updated))