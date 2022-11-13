module TypeChecker exposing (..)

import Ast.Func exposing (Func)
import Ast.Instruction exposing (ControlScope, FoldedInstr(..), IfScope, Instruction(..))
import Ast.Module exposing (Ast)
import Dict exposing (Dict)
import More.List as List
import ValType exposing (ValType(..))


type alias ScopeEnv =     
        { label : Maybe String
        , result : Maybe ValType
        , isLoop : Bool
        , unreachable : Bool
        }


type alias FuncEnv =
    { result : Maybe ValType
    , functions : Dict String (List ValType, Maybe ValType)
    , locals : Dict String ValType
    , stack : List ValType
    , scopes : List ScopeEnv 
    , unreachable : Bool
    }


pop : List ValType -> FuncEnv -> Result String FuncEnv    
pop params env = 
    case params of
        [] -> Ok env
        head :: tail ->
            case pop tail env of
                Ok newEnv ->
                    case newEnv.stack of
                        [] -> 
                            Err "Tried to pop from an empty stack."
                        
                        stackHead :: stackTail ->
                            if newEnv.unreachable || head == stackHead then
                                Ok {newEnv | stack = stackTail}
                            else
                                "Expected " ++ ValType.toString head ++ ", got " ++ ValType.toString stackHead ++ "."
                                |> Err
                
                err -> err


op : List ValType -> Maybe ValType -> FuncEnv -> Result String FuncEnv 
op args result env =
    pop args env
    |> Result.map (\newEnv -> 
        case result of 
            Nothing -> newEnv
            Just r -> {newEnv | stack = r :: newEnv.stack}
    )

                                
binOp : ValType -> ValType -> ValType -> FuncEnv -> Result String FuncEnv
binOp p1 p2 ret func = 
    op [p1, p2] (Just ret) func


unOp : ValType -> ValType -> FuncEnv -> Result String FuncEnv
unOp param ret env = 
    op [param] (Just ret) env


const : ValType -> FuncEnv -> Result String FuncEnv
const val env =
    op [] (Just val) env


getLocalType : String -> FuncEnv -> Result String ValType
getLocalType label env = 
    case Dict.get label env.locals of
        Just t -> Ok t
        Nothing -> Err <| "No local $" ++ label ++ " defined in the current scope."


getLocalTypeThen : String -> FuncEnv -> (ValType -> Result String FuncEnv) -> Result String FuncEnv
getLocalTypeThen label env func =
    getLocalType label env
    |> Result.andThen func



checkInstr : Instruction -> FuncEnv -> Result String FuncEnv
checkInstr instr env =
    case instr of
        
        -- this is a certified Elm moment
        F32Abs -> unOp F32 F32 env
        F32Neg -> unOp F32 F32 env
        F32Sqrt -> unOp F32 F32 env
        F32Ceil -> unOp F32 F32 env
        F32Floor -> unOp F32 F32 env
        F32Trunc -> unOp F32 F32 env
        F32Nearest -> unOp F32 F32 env
        
        F64Abs -> unOp F64 F64 env
        F64Neg -> unOp F64 F64 env
        F64Sqrt -> unOp F64 F64 env
        F64Ceil -> unOp F64 F64 env
        F64Floor -> unOp F64 F64 env
        F64Trunc -> unOp F64 F64 env
        F64Nearest -> unOp F64 F64 env
        
        I32Add -> binOp I32 I32 I32 env
        I32Sub -> binOp I32 I32 I32 env
        I32Mul -> binOp I32 I32 I32 env
        I32And -> binOp I32 I32 I32 env
        I32Or -> binOp I32 I32 I32 env
        I32Xor -> binOp I32 I32 I32 env
        
        I64Add -> binOp I64 I64 I64 env
        I64Sub -> binOp I64 I64 I64 env
        I64Mul -> binOp I64 I64 I64 env
        I64And -> binOp I64 I64 I64 env
        I64Or -> binOp I64 I64 I64 env
        I64Xor -> binOp I64 I64 I64 env
        
        F32Add -> binOp F32 F32 F32 env
        F32Sub -> binOp F32 F32 F32 env
        F32Mul -> binOp F32 F32 F32 env
        F32Min -> binOp F32 F32 F32 env
        F32Max -> binOp F32 F32 F32 env
        
        F64Add -> binOp F64 F64 F64 env
        F64Sub -> binOp F64 F64 F64 env
        F64Mul -> binOp F64 F64 F64 env
        F64Min -> binOp F64 F64 F64 env
        F64Max -> binOp F64 F64 F64 env
        
        I32Eqz -> unOp I32 I32 env
        I64Eqz -> unOp I64 I64 env
        
        I32Eq -> binOp I32 I32 I32 env
        I32Ne -> binOp I32 I32 I32 env
        I32LtS -> binOp I32 I32 I32 env
        I32GtS -> binOp I32 I32 I32 env
        I32LeS -> binOp I32 I32 I32 env
        I32GeS -> binOp I32 I32 I32 env
        
        I64Eq -> binOp I64 I64 I32 env
        I64Ne -> binOp I64 I64 I32 env
        I64LtS -> binOp I64 I64 I32 env
        I64GtS -> binOp I64 I64 I32 env
        I64LeS -> binOp I64 I64 I32 env
        I64GeS -> binOp I64 I64 I32 env
        
        F32Eq -> binOp F32 F32 I32 env
        F32Ne -> binOp F32 F32 I32 env
        F32Lt -> binOp F32 F32 I32 env
        F32Gt -> binOp F32 F32 I32 env
        F32Le -> binOp F32 F32 I32 env
        F32Ge -> binOp F32 F32 I32 env
        
        F64Eq -> binOp F64 F64 I32 env
        F64Ne -> binOp F64 F64 I32 env
        F64Lt -> binOp F64 F64 I32 env
        F64Gt -> binOp F64 F64 I32 env
        F64Le -> binOp F64 F64 I32 env
        F64Ge -> binOp F64 F64 I32 env
        
        LocalGet label ->
            getLocalTypeThen label env (\t -> op [] (Just t) env)
                
        LocalSet label -> 
            getLocalTypeThen label env (\t -> op [t] Nothing env)
        
        LocalTee label ->
            getLocalTypeThen label env (\t -> op [t] (Just t) env) -- todo : replace with peek function?
            
        Nop -> 
            Ok env
        
        Unreachable -> 
            Ok {env | unreachable = True}
        
        Loop loop -> 
            checkScope loop True env
                
        Block block ->
            checkScope block False env
        
        If if_ ->
            checkIf if_ env
                
        Br label ->
            checkBranch label True env
            
        BrIf label ->
            checkBranch label False env
        
        Return ->
            op (List.fromMaybe env.result) Nothing env
            |> Result.map (\x -> {x | unreachable = True})
            
        Call label ->
            case Dict.get label env.functions of
                Just (params, result) ->
                    op params result env
                
                Nothing ->
                    "Tried to call nonexistent function $" ++ label ++ "."
                    |> Err
                    
        I32Const _ -> const I32 env
        I64Const _ -> const I64 env
        F32Const _ -> const F32 env
        F64Const _ -> const F64 env

        Folded folded ->
            checkFolded folded env
        
        
checkFolded : FoldedInstr -> FuncEnv -> Result String FuncEnv
checkFolded folded env =
    case folded of 
        FInstr (head, tail) ->
            List.foldLeftUntilErr checkFolded tail env
            |> Result.andThen (checkInstr head)
        
        FIf if_ ->
            ( case if_.folded of
                Nothing -> Ok env
                Just subFolded -> 
                    checkFolded subFolded env 
            )
            |> Result.andThen (\x -> 
                let
                    unfoldedIf = -- todo : this is kind of redundant
                        { label = if_.label
                        , result = if_.result
                        , thenBlock = if_.thenBlock
                        , elseBlock = if_.elseBlock
                        }
                in 
                checkIf unfoldedIf x
            )
        
        FLoop loop ->
            checkScope loop True env
            
        FBlock block -> 
            checkScope block False env


checkIf : IfScope -> FuncEnv -> Result String FuncEnv
checkIf if_ env =
    case op [I32] Nothing env of
        Ok newEnv ->
            let 
                thenScope = {label = if_.label, result = if_.result, body = if_.thenBlock}
                thenResult = checkScope thenScope False newEnv
                elseScope = {label = if_.label, result = if_.result, body = if_.elseBlock} 
                elseResult = checkScope elseScope False newEnv
            in
            case (thenResult, elseResult) of
                (Ok _, Ok _) -> thenResult -- you can use either if they are both okay
                (Err _ as err, Ok _) -> err
                (Ok _, Err _ as err) -> err
                (Err e1, Err e2) -> Err (e1 ++ "\n" ++ e2) -- glob the errors together
        
        err -> err


checkBranch : String -> Bool -> FuncEnv -> Result String FuncEnv
checkBranch label unreachableAfter env = 
    let 
        isLabel scope =
            case scope.label of
                Nothing -> False
                Just x -> x == label
    in
    case List.first isLabel env.scopes of
        Nothing ->
            "Tried to branch to the nonexistent label $" ++ label ++ "."
            |> Err
                
        Just scope -> 
            if scope.isLoop then 
                Ok {env | unreachable = unreachableAfter}
            else
                op (List.fromMaybe scope.result) Nothing env
                |> Result.map (\x -> {x | unreachable = unreachableAfter})


checkScope : ControlScope -> Bool -> FuncEnv -> Result String FuncEnv
checkScope scope isLoop env =
    let
        pushedScope = 
            { label = scope.label
            , result = scope.result
            , isLoop = isLoop
            , unreachable = env.unreachable
            }
            
        newScope = 
            {env 
            | unreachable = False
            , stack = []
            , scopes = pushedScope :: env.scopes
            }
    in
    case checkInstructions scope.body newScope of
        Ok _ -> op [] scope.result env
        Err e -> Err e
        
        
checkInstructions : List Instruction -> FuncEnv -> Result String FuncEnv
checkInstructions instructions env =
    List.foldLeftUntilErr checkInstr instructions env
    

-- checkFunction : Func -> Dict String (List ValType, Maybe ValType) -> Maybe String
-- checkFunction fn funcSignatures =
--     let 
--         env : FuncEnv
--         env = 
--             { result = fn.result
--             , functions = funcSignatures
--             , locals = fn.params    
--             , stack = []
--             , scopes = []
--             , unreachable = False
--             }
--     in
--     
-- 
-- checkModule : Ast -> Maybe String
-- checkModule mod =
--     let functions = List.map (\fn -> (fn.label, (fn.params, fn.result))) mod.functions in
    