{-# LANGUAGE LambdaCase #-}
module Evaluate (eval) where

import Data.Functor
import Control.Monad.State
import qualified Data.Map.Strict as Map
import DataStructure

--- Run Evaluation ----

-- | given a MathExp, evaluate it.
--   if evaluation fails, provide the error message.
eval :: MathExp -> Either String Number
eval expr =
    let (t, env) = runState (eval_ expr) emptyEnv
    in case runErrorMsg env of
        "" -> Right $ runResult t
        errorMsg -> Left errorMsg

-- | if the final result is not a number value, show error message
eval_ :: MathExp -> CalcState CalcResult
eval_ expr =
    put emptyEnv *>
    evalExpr expr >>= \case
        Result res -> provideResult $ Result res
        ResultBool b -> cannotEvalLogicExpr b
        _ -> tooFewArgs


---- Prelude Plus ----

--- link a binary function to a unary function
(...) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(...) = (.) . (.)
infixr 8 ...


---- CalcState ----

type CalcState = State CalcEnv


---- CalcEnv ----

data CalcEnv = Env {
    -- the current lambda expression that is being handled
    -- used for providing error message
    runLambda :: String,
    -- error message
    runErrorMsg :: String, 
    -- let bindings
    -- function variables
    runBinds :: Bindings}

emptyEnv :: CalcEnv
emptyEnv = Env "" "" Map.empty

getLambda :: CalcState String
getLambda = gets runLambda 

putLambda :: MathExp -> CalcState ()
putLambda expr = modify $ \env ->
    case runLambda env of
        "" ->  env {runLambda = show expr}
        _ -> env

deleteLambda :: CalcState ()
deleteLambda = modify $ \env -> env {runLambda = ""}

getErrorMsg :: CalcState String
getErrorMsg = gets runErrorMsg

putErrorMsg :: String -> CalcState CalcResult
putErrorMsg errorMsg = modify (\env ->
    case runErrorMsg env of
        "" ->  env {runErrorMsg = errorMsg}
        _ -> env
    )$> emptyResult


---- Bindings ----

type Bindings = Map.Map Name CalcResult

modifyBinds :: (Bindings -> Bindings) -> CalcState ()
modifyBinds f = modify $ \env ->
    let newBinds = f . runBinds $ env
    in env {runBinds = newBinds}

getBinds :: CalcState Bindings
getBinds = gets runBinds

addBinds :: [Name] -> [CalcResult] -> CalcState ()
addBinds = modifyBinds ... inserts
    where inserts = Map.union . Map.fromList ... zip

deleteBinds :: [Name] -> CalcState ()
deleteBinds = modifyBinds . flip (foldr Map.delete)


---- CalcResult ----

-- | an overarching type of numbers and lambda expressions.
data CalcResult
    -- normal number
    = Result {runResult :: Number}
    | ResultBool {runResultBool :: Bool}
    -- handling function created by lambda expressions
    | Func {runFunc :: CalcResult -> CalcResult}

putFunc :: (CalcResult -> CalcResult) -> CalcState CalcResult
putFunc = pure . Func

emptyResult :: CalcResult
emptyResult = Result 0

-- | after successfully solving a function,
--   clear the lambda expression stored for possible error messages.
provideResult :: CalcResult -> CalcState CalcResult
provideResult t = deleteLambda $> t


---- Application ----

-- | deals with preorder expressions connected by ' '
evalApp :: Application -> CalcState CalcResult
evalApp (Ground expr) = evalExpr expr
evalApp (Apply app1 app2) = do
    t1 <- evalApp app1
    t2 <- evalApp app2
    case t1 of
        Func f -> return $ f t2
        Result res -> tooManyArgs res
        ResultBool b -> cannotEvalLogicExpr b


---- MathExp ----

-- | interrupt the evaluation if an error has already been detected
evalExpr :: MathExp -> CalcState CalcResult
evalExpr expr =
    getErrorMsg >>= \case
        "" -> evalExpr_ expr
        _ -> pure emptyResult

evalExpr_ :: MathExp -> CalcState CalcResult
evalExpr_ (Number n) = pure $ Result n
evalExpr_ (Var name) = do
    binds <- getBinds
    case binds Map.!? name of
        Just val -> return val
        Nothing -> putErrorMsg $ "Variable " ++ name ++ " is not in the scope."
evalExpr_ (Val app) = evalApp app
evalExpr_ (Let vars rawVals expr) = do
    vals <- mapM evalExpr rawVals
    evalWithBinds vars vals expr
evalExpr_ expr@(Lambda arg func) = do
    env <- get
    putLambda expr
    putFunc $ \s ->
        evalWithBinds [arg] [s] func `evalState` env
evalExpr_ (Cond cond) = ResultBool <$> evalCond cond
evalExpr_ (If cond expr1 expr2) =
    evalExpr cond >>= \case
        ResultBool condRes -> 
            evalExpr $ if condRes
                then expr1
                else expr2
        _ -> putErrorMsg $ "Condition " ++ show cond ++ " is not a logical expression." 
evalExpr_ (Neg   expr) = evalUnary negate expr
evalExpr_ (Plus  expr1 expr2) = evalBinary (+)  expr1 expr2
evalExpr_ (Minus expr1 expr2) = evalBinary (-)  expr1 expr2
evalExpr_ (Mult  expr1 expr2) = evalBinary (*)  expr1 expr2
evalExpr_ (Div   expr1 expr2) = evalBinary quot expr1 expr2
evalExpr_ (Pow   expr1 expr2) = evalBinary (^)  expr1 expr2

evalWithBinds :: [Name] -> [CalcResult] -> MathExp -> CalcState CalcResult
evalWithBinds vars vals expr = addBinds vars vals *> evalExpr expr <* deleteBinds vars

evalUnary :: Unary Number -> MathExp -> CalcState CalcResult
evalUnary op expr = do
    t <- evalExpr expr
    case t of
        Result res -> provideResult . Result $ op res
        ResultBool b -> cannotEvalLogicExpr b
        _ -> tooFewArgs

evalBinary :: Binary Number -> MathExp -> MathExp -> CalcState CalcResult
evalBinary op expr1 expr2 = do
    t1 <- evalExpr expr1 
    t2 <- evalExpr expr2
    case (t1, t2) of
        (Result res1, Result res2) -> provideResult . Result $ res1 `op` res2
        (ResultBool b1, _) -> cannotEvalLogicExpr b1
        (_, ResultBool b2) -> cannotEvalLogicExpr b2
        _ -> tooFewArgs


---- Condition ----

evalCond :: Condition -> CalcState Bool
evalCond (Boolean bool) = return bool
evalCond (Not cond) = not <$> evalCond cond
evalCond (And cond1 cond2) = (&&) <$> evalCond cond1 <*> evalCond cond2
evalCond (Or  cond1 cond2) = (||) <$> evalCond cond1 <*> evalCond cond2
evalCond (E  expr1 expr2) = evalCompare (==) expr1 expr2
evalCond (NE expr1 expr2) = evalCompare (/=) expr1 expr2
evalCond (G  expr1 expr2) = evalCompare (> ) expr1 expr2
evalCond (L  expr1 expr2) = evalCompare (< ) expr1 expr2
evalCond (GE expr1 expr2) = evalCompare (>=) expr1 expr2
evalCond (LE expr1 expr2) = evalCompare (<=) expr1 expr2

evalCompare :: (Number -> Number -> Bool) -> MathExp -> MathExp -> CalcState Bool
evalCompare op expr1 expr2 = do
    t1 <- evalExpr expr1 
    t2 <- evalExpr expr2
    case (t1, t2) of
        (Result res1, Result res2) -> return $ res1 `op` res2
        (ResultBool b1, _) -> cannotEvalLogicExpr b1 $> False
        (_, ResultBool b2) -> cannotEvalLogicExpr b2 $> False
        _ -> tooFewArgs $> False


---- Error Message ----

tooFewArgs :: CalcState CalcResult
tooFewArgs = do
    l <- getLambda
    putErrorMsg $ "Expression " ++ l ++ " applied to too few arguments."

tooManyArgs :: Number -> CalcState CalcResult
tooManyArgs n = putErrorMsg $ "Expression " ++ show n ++ " applied to too many arguments."

cannotEvalLogicExpr :: Bool -> CalcState CalcResult
cannotEvalLogicExpr b = putErrorMsg $ "Logic value " ++ show b ++ " can not be numerically evaluated."