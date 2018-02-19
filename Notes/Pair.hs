module Pair where

--1. Define the abstratct Syntax

data Expr = LitI Int 
                | LitB Bool
                | Pair Expr Expr
                | Fst Expr
                | Snd Expr
                | Swap Expr
            deriving(Eq, Show)
 
 -- some examples View the lecture notes online to view these examples.
 
 -- 2. Define the semantic domain.
 
 data Value = I Int
                    | B Bool
                    | P Value Value

-- Domain: Maybe Value

-- 3. Define our evaluation function.

sem:: Exp -> Maybe Value
sem (LitI i)     = Just (I i)
sem (LitB b)   = Just (B b)
sem (Pair l r)  =  case (sem l, sem r) of
                           (Just lv, Just rv) -> Just (P lv, rv)
                           _-> Nothing
sem (Fst e)     =   case sem e of
                                    Just (P lv rv) -> Just lv
                                    _-> Nothing
sem (Snd e)   = case sem e of
                                    Just (P lv rv) -> Just rv
                                    _-> Nothing
sem (Swap e) = case sem e of
                                        Just(P lv rv) -> Just (P rv lv)

                                        
-- 4. Define the strucrure of types

data Type = TInt
                   | TBool
                   | TPair Type Type