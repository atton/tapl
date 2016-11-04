module SimpleBool where

import Control.Applicative
import Data.Either

type Index       = Int
type ContextSize = Int
type Name        = String
type Error       = String
type EitherE     = Either Error

data Binding = NameBind
               | VarBind Type
               deriving (Show, Eq)
type Context = [(Name, Binding)]

data Type  = TyArr Type Type
           | TyBool
           deriving (Show, Eq)

data Term = TmVar Index ContextSize
          | TmAbs Name Type Term
          | TmApp Term Term
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          deriving Show


index2name ctx i = "hoge"


addBinding :: Context -> Name-> Binding -> Context
addBinding ctx n b = (n, b) : ctx

getBinding :: Context -> Index -> EitherE Binding
getBinding ctx i
  | length ctx < i = Left "Index Error."
  | otherwise      = return . snd $ ctx !! i


getTypeFromContext :: Context -> Index -> EitherE Type
getTypeFromContext ctx i = getTypeFromBinding $ getBinding ctx i
  where
    getTypeFromBinding (Right (VarBind t)) = return t
    getTypeFromBinding _                   = Left $ "getTypeFromContext: Wrong kind of binding for variable " ++ index2name ctx i


typeOf :: Context -> Term -> EitherE Type
typeOf ctx (TmVar i _)      = getTypeFromContext ctx i
typeOf ctx (TmAbs x ty tm2) = let ctx' = addBinding ctx x (VarBind ty)
                              in  (typeOf ctx' tm2) >>= (\ty2 -> return $ TyArr ty ty2)
typeOf ctx (TmApp tm1 tm2)  = do
                                ty1 <- typeOf ctx tm1
                                ty2 <- typeOf ctx tm2
                                case ty1 of
                                  (TyArr ty11 ty12) -> if ty2 == ty11 then return ty12
                                                                      else Left "Parameter type mismatch."
typeOf ctx (TmTrue)           = return TyBool
typeOf ctx (TmFalse)          = return TyBool
typeOf ctx (TmIf tm1 tm2 tm3)
  | (typeOf ctx tm1) == (Right TyBool) = do ty2 <- typeOf ctx tm2
                                            ty3 <- typeOf ctx tm3
                                            if (ty2 == ty3) then return ty2
                                                            else Left "arms of conditional have different types"
  | otherwise              = Left "guard of conditional not a boolean"


sampleContext1 = [("x" , VarBind TyBool)]
sampleTerm1    = TmVar 0 1

sampleContext2 = []
sampleTerm2    = TmIf TmTrue (TmAbs "x" TyBool (TmVar 0 1)) (TmAbs "y" TyBool TmFalse)

sampleContext3 = []
sampleTerm3    = TmAbs "x" TyBool (TmVar 0 1)

sampleContext4 = []
sampleTerm4    = TmApp (TmAbs "x" (TyArr TyBool TyBool) (TmIf (TmApp (TmVar 0 1) TmFalse) TmTrue TmFalse)) (TmAbs "x" TyBool (TmIf (TmVar 0 1) TmFalse TmTrue))

sampleContext5 = []
sampleTerm5    = TmIf (TmAbs "x" TyBool TmFalse) TmTrue TmFalse
