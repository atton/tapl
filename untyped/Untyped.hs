module Untyped where

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

-- my implementation
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

-- from book
termShift :: Int -> Term -> Term
termShift d t = walk 0 t
  where
    walk c (TmVar x n)
      | x >= c    = TmVar (x+d) (n+d)
      | otherwise = TmVar x (n+d)
    walk c (TmAbs x t1)  = TmAbs x (walk (c+1) t1)
    walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

