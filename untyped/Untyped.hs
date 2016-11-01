module Untyped where

import Data.Either
import Data.Functor

type Index       = Int
type ContextSize = Int
type Name        = String
data Binding     = NameBind deriving Show
type Context     = [(Name, Binding)]

data Term = TmVar Index ContextSize |
            TmAbs String Term       |
            TmApp Term Term
            deriving Show

-- my implementation
index2name :: Context -> Index -> String
index2name ctx i = fst $ ctx !! i

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName ctx n = (((freshName, NameBind) : ctx), freshName)
        where
            freshName = head . dropWhile (flip elem $ map fst ctx) $ iterate (++ "'") n


-- from book
showTerm :: Context -> Term -> String
showTerm ctx (TmAbs x t)   = let (ctx', x') = pickFreshName ctx x
                             in "(lambda " ++ x' ++ ". " ++ (showTerm ctx' t) ++ ")"
showTerm ctx (TmApp t1 t2) = "(" ++ (showTerm ctx t1) ++ " " ++ (showTerm ctx t2) ++ ")"
showTerm ctx (TmVar i n)
  | length ctx == n = index2name ctx i
  | otherwise     = "[bad index]"

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
  where
    walk c (TmVar x n)
      | x >= c    = TmVar (x+d) (n+d)
      | otherwise = TmVar x (n+d)
    walk c (TmAbs x t1)  = TmAbs x (walk (c+1) t1)
    walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubst :: Index -> Term -> Term -> Term
termSubst j s t = walk 0 t
  where
    walk c (TmVar x n)
      | x == (j+c) = termShift c s
      | otherwise  = TmVar x n
    walk c (TmAbs x t1)  = TmAbs x (walk (c+1) t1)
    walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: Context -> Term -> Bool
isVal ctx (TmAbs _ _) = True
isval _    _          = False

eval1 :: Context -> Term -> Either String Term
eval1 ctx (TmApp (TmAbs x t12) v2)
  | isVal ctx v2 = return $ termSubstTop v2 t12
  | otherwise    = Left $ show v2 ++ " is not variable."
eval1 ctx (TmApp t1 t2)
  | isVal ctx t1 = eval1 ctx t2 >>= (\t -> return $ TmApp t1 t)
  | otherwise    = eval1 ctx t1 >>= (\t -> return $ TmApp t t2)
eval1 _ _ = Left "No Rule Applies"


-- x
sampleContext1 = [("x", NameBind)]
sampleTerm1    = TmVar 0 1

-- \y -> y
sampleContext2  = []
sampleTerm2     = TmAbs "y" (TmVar 0 1)

-- (\x -> x) (\x -> x x) == \x' -> x' x'
sampleContext3 = []
sampleTerm3    = (TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "x" (TmApp (TmVar 0 1) (TmVar 0 1))))

-- (\y -> (\x -> y)) (\x -> x x)
sampleContext4 = []
sampleTerm3    = (TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "x" (TmApp (TmVar 0 1) (TmVar 0 1))))
sampleTerm4    = TmApp (TmAbs "y" (TmAbs "x" (TmVar 1 2))) (TmAbs "x" (TmApp (TmVar 0 1) (TmVar 0 1)))
