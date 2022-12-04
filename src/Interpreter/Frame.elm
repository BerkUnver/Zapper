module Interpreter.Frame exposing 
    ( Frame
    , RuntimeErr(..)
    , new
    , getLocal
    , setLocal
    , peek
    , pop
    , push
    , getStack
    , pushScope
    , expectI32
    , expectI64
    , expectF32
    , expectF64
    , unOpI32
    , unOpI64
    , unOpF32
    , unOpF64
    , binOpI32
    , binOpI64
    , binOpF32
    , binOpF64
    , relOpF32
    , relOpF64
    )


import Dict exposing (Dict)
import Interpreter.Frame as Frame
import Interpreter.Val as Val exposing (Val)


type Scope = Scope 
    { label : Maybe String
    , stack : List Val
    , isLoop : Bool    
    }


type Frame = Frame
    { locals : Dict String Val
    , stack : List Val
    , scopes : List Scope
    }


type RuntimeErr
    = TypeErr String
    | StackEmptyErr String
    | LabelErr String


new : Dict String Val -> Frame
new locals = 
    Frame {locals = locals, scopes = [], stack = []}
    

getLocal : String -> Frame -> Result RuntimeErr Val
getLocal label (Frame frame) = 
    Dict.get label frame.locals
    |> Result.fromMaybe (LabelErr label)


setLocal : String -> Val -> Frame -> Result RuntimeErr Frame
setLocal label val (Frame frame as f) =
    getLocal label f
    |> Result.andThen (\local -> 
        if Val.sameType val local then
            Ok <| Frame {frame | locals = Dict.insert label val frame.locals}
        else
            "Expected local to be of type " 
            ++ Val.typeToString val 
            ++ " but it was of type " 
            ++ Val.typeToString local
            |> TypeErr |> Err
    )


peek : Frame -> Result RuntimeErr Val
peek (Frame frame) = 
    case frame.stack of
        [] -> Err <| StackEmptyErr "Tried to peek from an empty stack."
        head :: _ -> Ok head


pop : Frame -> Result RuntimeErr (Val, Frame)
pop (Frame frame) = 
    case frame.stack of
        [] -> 
            Err <| StackEmptyErr "Tried to pop from an empty stack."
        head :: tail -> 
            Ok (head, Frame {frame | stack = tail})


push : Val -> Frame -> Frame
push val (Frame frame) = 
    Frame {frame | stack = val :: frame.stack}


-- todo : Find a way to get rid of this horribleness

getStack : Frame -> List Val
getStack (Frame frame) = frame.stack


pushScope : Maybe String -> Bool -> Frame -> Frame
pushScope label isLoop (Frame frame) =
    {frame 
    | stack = []
    , scopes = Scope 
        { label = label
        , stack = frame.stack
        , isLoop = isLoop
        } :: frame.scopes
    }
    |> Frame


expectI32 frame =
    pop frame
    |> Result.andThen (\(x, fr) -> 
        case x of
            Val.I32 i -> Ok (i, fr)
            t -> Err <| TypeErr <| "expected i32, got " ++ Val.typeToString t ++ "."
    )
        

expectI64 frame = 
    pop frame
    |> Result.andThen (\(x, fr) ->
        case x of
            Val.I64 i -> Ok (i, fr)
            t -> Err <| TypeErr <| "expected i64, got " ++ Val.typeToString t ++ "."
        )


expectF32 frame = 
    pop frame
    |> Result.andThen (\(x, fr) ->
        case x of
            Val.F32 f -> Ok (f, fr)
            t -> Err <| TypeErr <| "expected f32, got " ++ Val.typeToString t ++ "."
        )


expectF64 frame = 
    pop frame
    |> Result.andThen (\(x, fr) ->
        case x of
            Val.F64 f -> Ok (f, fr)
            t -> Err <| TypeErr <| "expected f64, got " ++ Val.typeToString t ++ "."
        )


unOpI32 : (Int -> Int) -> Frame -> Result RuntimeErr Frame
unOpI32 op frame = 
    Frame.expectI32 frame
    |> Result.map (\(i32, fr) -> Frame.push (Val.I32 <| op i32) fr)


unOpI64 : (Int -> Int) -> Frame -> Result RuntimeErr Frame
unOpI64 op frame = 
    Frame.expectI64 frame
    |> Result.map (\(i64, fr) -> Frame.push (Val.I64 <| op i64) fr)


unOpF32 : (Float -> Float) -> Frame -> Result RuntimeErr Frame
unOpF32 op frame = 
    Frame.expectF32 frame
    |> Result.map (\(f32, fr) -> Frame.push (Val.F32 <| op f32) fr)


unOpF64 : (Float -> Float) -> Frame -> Result RuntimeErr Frame
unOpF64 op frame =
    Frame.expectF64 frame
    |> Result.map (\(f64, fr) -> Frame.push (Val.F64 <| op f64) fr)
    

binOpI32 : (Int -> Int -> Int) -> Frame -> Result RuntimeErr Frame
binOpI32 op frame =
    expectI32 frame
    |> Result.andThen (\(rhs, f1) ->
        expectI32 f1
        |> Result.map (\(lhs, fr) ->
        push (op lhs rhs |> Val.I32) fr))
        

binOpI64 : (Int -> Int -> Int) -> Frame -> Result RuntimeErr Frame
binOpI64 op frame =
    expectI64 frame
    |> Result.andThen (\(rhs, f1) ->
        expectI64 f1
        |> Result.map (\(lhs, fr) ->
        push (op lhs rhs |> Val.I64) fr))
        
        
binOpF32 : (Float -> Float -> Float) -> Frame -> Result RuntimeErr Frame
binOpF32 op frame =
    expectF32 frame
    |> Result.andThen (\(rhs, f1) ->
        expectF32 f1
        |> Result.map (\(lhs, fr) ->
        push (op lhs rhs |> Val.F32) fr))
        
        
binOpF64 : (Float -> Float -> Float) -> Frame -> Result RuntimeErr Frame
binOpF64 op frame =
    expectF64 frame
    |> Result.andThen (\(rhs, f1) ->
        expectF64 f1
        |> Result.map (\(lhs, fr) ->
        push (op lhs rhs |> Val.F64) fr))
        
        
relOpF32 : (Float -> Float -> Int) -> Frame -> Result RuntimeErr Frame
relOpF32 op frame =
    expectF32 frame
    |> Result.andThen (\(rhs, f1) ->
        expectF32 f1
        |> Result.map (\(lhs, fr) ->
        push (op lhs rhs |> Val.I32) fr))
        
        
relOpF64 : (Float -> Float -> Int) -> Frame -> Result RuntimeErr Frame
relOpF64 op frame =
    expectF64 frame
    |> Result.andThen (\(rhs, f1) ->
        expectF64 f1
        |> Result.map (\(lhs, fr) ->
        push (op lhs rhs |> Val.I32) fr))
        
