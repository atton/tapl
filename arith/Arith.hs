module Arith where

import Data.Either
import Control.Monad.Trans.Writer

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving Show


isNumerical :: Term -> Bool
isNumerical TmZero     = True
isNumerical (TmSucc t) = isNumerical t
isNumerical (TmPred t) = isNumerical t
isNumerical _          = False

isVal :: Term -> Bool
isVal TmTrue  = True
isVal TmFalse = True
isVal t
  | isNumerical t = True
  | otherwise     = False


eval1 :: Term -> Either String Term
eval1 (TmIf TmTrue  t _)    = return t
eval1 (TmIf TmFalse _ f)    = return f
eval1 (TmIf con t f)        = eval1 con >>= (\con' -> return (TmIf con' t f))
eval1 (TmSucc (TmPred t))   = return t
eval1 (TmSucc t)            = eval1 t >>= (\t' -> return $ TmSucc t')
eval1 (TmPred TmZero)       = return $ TmZero
eval1 (TmPred (TmSucc t))
  | isNumerical t           = return t
  | otherwise               = eval1 t >>= (\t' -> return $ TmSucc (TmPred t'))
eval1 (TmIsZero TmZero)     = return $ TmTrue
eval1 (TmIsZero (TmSucc t))
  | isNumerical t           = return $ TmFalse
  | otherwise               = eval1 (TmSucc t) >>= (\t' -> return $ TmIsZero t')
eval1 (TmIsZero t)          = eval1 t >>= (\t' -> return $ TmIsZero t')
eval1 t                     = Left $ "Cannot eval: " ++ show t


evalWithWriter :: Term -> Writer String ()
evalWithWriter t = do
  tell $ (show t) ++ "\n"
  case (eval1 t) of
     (Right t') -> evalWithWriter t'
     (Left  s)  -> tell $ s

evalWithStep :: Term -> String
evalWithStep = execWriter . evalWithWriter



-- Terms from arith/test.f

sampleTerm1 = TmTrue
sampleTerm2 = TmIf TmFalse TmTrue TmFalse
sampleTerm3 = TmZero
sampleTerm4 = TmSucc (TmPred TmZero)
sampleTerm5 = TmIsZero (TmPred (TmSucc (TmSucc TmZero)))
