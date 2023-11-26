{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe

-- Murashko Artem BS20-02
-- HW6

-- | 1 Sample expressions | --

-- | A simple expression with variables.
data Expr a
  = LitInt Integer        -- ˆ Integer Literal.
  | LitBool Bool          -- ˆ Booll Literal
  | Var a                 -- ˆ Variable
  | Add (Expr a) (Expr a) -- ˆ Addition.
  | Mul (Expr a) (Expr a) -- ˆ Multiplication.
  | And (Expr a) (Expr a) -- ˆ Logical AND.
  deriving (Show, Functor)

sampleExpr1 :: Expr String
sampleExpr1 = Add (LitInt 1) (Mul (Var "x") (Var "width"))

sampleExpr2 :: Expr Int
sampleExpr2 = Add (LitInt 1) (Mul (Var 0) (Var 1))

-- | Num instance for nice syntax.
instance Num (Expr a) where
  (+) :: Expr a -> Expr a -> Expr a
  e1 + e2 = Add e1 e2

  (*) :: Expr a -> Expr a -> Expr a
  e1 * e2 = Mul e1 e2

  fromInteger n = LitInt n
  -- NOTE: there are more methods, but we leave them undefined for now

sampleExpr1' :: Expr String
sampleExpr1' = 1 + x * width
  where
    x = Var "x"
    width = Var "width"

sampleExpr2' :: Expr Integer
sampleExpr2' = 1 + Var 0 * Var 1

-- Some variables
x = Var "x"
y = Var "y"
z = Var "z"

-- | Evaluate an expression with all variables instantiated.
eval :: Expr Integer -> Integer
eval (LitInt n) = n
eval (LitBool n)
  | n = 1
  | not n = 0
eval (Var n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (And e1 e2) = error "Cannot evaluate logical AND in the context of Expr Integer"

-- | Evaluate an expression with all variables instantiated.
evalBool :: Expr Bool -> Bool
evalBool (LitInt n) 
  | n == 0    = False
  | otherwise = True 
evalBool (LitBool n) = n
evalBool (Var n) = n
evalBool (Add e1 e2) = error "Cannot evaluate Add in the context of Expr Bool"
evalBool (Mul e1 e2) = error "Cannot evaluate Mul in the context of Expr Bool"
evalBool (And e1 e2) = evalBool e1 && evalBool e2

-- | Display an expression with variables.
display :: Expr String -> String
display (LitInt n) = show n
display (LitBool n) = show n
display (Var s) = s
display (Add e1 e2) = display e1 ++ " + " ++ display e2
display (Mul e1 e2) = "(" ++ display e1 ++ ") * (" ++ display e2 ++ ")"
display (And e1 e2) = "(" ++ display e1 ++ ") & (" ++ display e2 ++ ")"

-- | 1.1 Configurable evaluation and display

-- | Evaluate an expression using an associative list
-- to lookup variable values and a default value
-- (in case there is no value in associative list).
evalWith :: Eq var => Int -> [(var, Int)] -> Expr var -> Int
evalWith defaultVal varList expr = fromInteger $ eval $ fmap lookupVar expr
  where
    lookupVar v = case lookup v varList of
      Just val -> toInteger val
      Nothing -> fromIntegral defaultVal

-- | Display an expression using a given
-- display function for variables.
displayWith :: (var -> String) -> Expr var -> String
displayWith displayVar expr = display $ fmap displayVar expr

-- | 1.2 Subexpression substitution

-- | Substitutes every variable with its corresponding expression value
expandVars :: (Eq b, Show a) => Expr a -> [(b, Expr a)] -> Expr b -> Expr a
expandVars _ _ (LitInt n) = LitInt n
expandVars _ _ (LitBool n) = LitBool n
expandVars uv vars (Var s) = fromMaybe uv (lookup s vars)
expandVars uv vars (Add e1 e2) = Add (expandVars uv vars e1) (expandVars uv vars e2)
expandVars uv vars (Mul e1 e2) = Mul (expandVars uv vars e1) (expandVars uv vars e2)
expandVars uv vars (And e1 e2) = And (expandVars uv vars e1) (expandVars uv vars e2)

-- | 1.3 Extensions

-- I have added Bool literan and logical And operation.