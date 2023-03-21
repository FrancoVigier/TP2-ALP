module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion x = conversionAux [] x

conversionAux :: [String] -> LamTerm -> Term
conversionAux xs x@(LVar _) = convertir xs x
conversionAux xs (App x y)  = (conversionAux xs x) :@: (conversionAux xs y)
conversionAux xs (Abs x y)  = let
                                (first, cont) = desglosar x
                                adentro | cont == "" = y
                                        | otherwise  = Abs cont y
                              in Lam (conversionAux (first:xs) adentro)

convertir :: [String] -> LamTerm -> Term
convertir xs (LVar x) = case (elemIndex x xs) of
                        Just i -> Bound i
                        Nothing -> Free (Global x)

desglosar :: String -> (String, String)
desglosar x = let
                f [] = []
                f (x:s) | x == ' ' = []
                        | otherwise = x:(f s)
                g [] = []
                g (x:s) | x == ' ' = s
                        | otherwise = g s
              in (f x, g x)

test10 = (conversion (Abs "y" (App (Abs "x y" (LVar "x")) (LVar "y")))) == (Lam (Lam (Lam (Bound 1)) :@: Bound 0))
test11 = (conversion (App (Abs "isCosa y" (LVar "isCosa")) (LVar "y"))) == (Lam (Lam (Bound 1)) :@: Free (Global "y"))

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) x = f x
vapp (VNeutral n) x = VNeutral (NApp n x)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv)     = lEnv !! ii
eval' (Free n)   (gEnv, _)     = let 
                                    rs = (filter (\(n', v) -> n' == n) gEnv)
                                 in case rs of 
                                    []         -> error "Non existent function used!"
                                    ((_, r):_) -> r
eval' (x :@: y)   e            = vapp (eval' x e) (eval' y e) 
eval' (Lam x)     (gEnv, lEnv) = VLam (\v -> eval' x (gEnv, v:lEnv))


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote v = quote' 0 v

quote' :: Int -> Value -> Term
quote' i (VLam f)                      = Lam (quote' (i+1) (f (VNeutral (NFree (Quote i)))))
quote' i (VNeutral (NFree (Global s))) = Free (Global s)
quote' i (VNeutral (NFree (Quote k)))  = Bound (i - k - 1)
quote' i (VNeutral (NApp  n v))        = ((quote' (i+1) (VNeutral n)) :@: (quote' (i+1) v))

testAux e x = quote (eval e (conversion x))

test2 = testAux [] (Abs "xaaa y" (LVar "xaaa")) == Lam (Lam (Bound 1))
test3 = testAux [(Global "r", VNeutral (NFree (Global "r")))] (App (Abs "x y" (LVar "x")) (LVar "r")) == Lam (Free (Global "r"))
test4 = testAux [(Global "r", VNeutral (NFree (Global "r"))), (Global "g", VNeutral (NFree (Global "g")))] (App (App (Abs "x y" (LVar "x")) (LVar "r")) (LVar "g")) == Free (Global "r")















