module Binding where

import Lambda
import Data.List (lookup)
import Control.Monad (foldM)

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1. 
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]

simplifyCtx ctx step expr = do
    expr' <- substituteMacros ctx expr
    return $ simplify step expr'

substituteMacros :: Context -> Lambda -> Either String Lambda

substituteMacros ctx (Var x) = Right $ Var x

substituteMacros ctx (App e1 e2) = do
    e1' <- substituteMacros ctx e1
    e2' <- substituteMacros ctx e2
    return $ App e1' e2'

substituteMacros ctx (Abs x e) = do
    e' <- substituteMacros ctx e
    return $ Abs x e'

substituteMacros ctx (Macro m) =
    case lookup m ctx of
        Just expr -> substituteMacros ctx expr
        Nothing   -> Left $ "Macro " ++ m ++ " not found in context"

-- 3.2.
normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
