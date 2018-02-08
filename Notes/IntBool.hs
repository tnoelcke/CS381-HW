-- | simple Lang with two types.

module IntBool where

--Syntax of the "Core" IntBool Lang

--int ::= (Any int)
--
--bool ::= true|false
-- exp ::= int
--      | exp + exp
--      | exp * exp
--      | exp = exp
--      | exp ? exp : exp

--1. Define the abstract syntax as haskeel data type.

data Exp = Lit Int
         |  Add Exp Exp
         |  Mul Exp Exp
         |  Equ Exp Exp
         |  If Exp Exp Exp
         deriving(Eq, Show)
  
-- Here are some examples expressions
-- *draw the abstract syntax trees (exercies)
-- * What should the result be?

ex1 = mul(Lit 2) (Add (Lit 3) (Lit 4))  --2*(3+4) => 14
ex2 = Equ ex1 (Lit 10)                  --ex1 == 10 => False
ex3 = If ex1 (Lit 5) (Lit 6)            --ex2 == ex1 ? 5:6 => Type Error! 

--2. Identify/define the semantic domain for this lang
--  * what types of values can we have?
--  *  How can we express this in Haskell?
--  * semantic domain - set of values a program can have.

-- defines the semantic domain or denominational semantics. This captures all the possible results
-- or our language.
data Value = I Int
           | B Bool
           | TypeError
           deriving(Eq, Show)
           
           
-- could have used Maybe = Just a | Nothing
-- Either a b = Left a | Right b


-- type Value = Maybe (Either Int Bool)
-- 
-- Powerful way to encode simatic domain.
-- I 14 <=> Just (Left 14)
-- B False <=> Just (Rihgt False)
-- TypeError <=> Nothing 


-- 3. Define the semantic function.
-- maps AST's to Values. Here we are giving meaning to to the lang.
sem :: Exp -> Value
sem (Lit i)    = I i
sem (Add l r)  = case (sem l, sem r) of
                   (I i, I j) -> I (i + j)
                   _ -> TypeError
sem (Mul l r)  = case (sem l, sem r) of
                    (I i, I j) -> I (i*j)
sem (Equ l r)  = case (sem l, sem r) of
                    (I i, I j) -> B ( i == j)
                    (B b, B c) -> B ( b == c)
                    _ -> TypeError
sem (If c t e) = case sem c of 
                    B b -> If b then sem t else sem e
                    _ -> TypeError
                    
-- We've created the semantics for this simple expression language.

-- 4. Syntactic sugar.
-- Goal: extend the syntax of our language with the following operation:
-- * boolean literals
-- * integer negation
-- * boolean negation (not)
-- * conjunction (and)
-- * disjunction (or)
-- 
-- implementing these in our simple language wouldn't be to bad but if our language was
-- large it would make writing the compiler much harder.
-- static sugar extends the syntax of the language with out changing the semantics. We are going
-- to produce Haskell functions that produce the AST.

true :: Exp
true = Equ (Lit 0) (Lit 0)

false :: Expr
false = Equ(Lit 1) (Lit 0)
 
 
 --stopped here last time picked up again on 2.7.2018
 --integer negation.
neg :: Exp -> Exp
neg e = Mul (Lit (-1)) e

 
 --boolean negation.
 -- example let e = Add (Lit 3) (Lit 4)
 -- neg e
 -- Mul (Lit(-1)) (Add (Lit 3) (Lit 4))
 -- Mul takes an AST and returns a negated AST.
not :: Exp -> Exp
not e = If e false true
 
 -- conjunction (And)
 -- if e evalutes to true return false else return true.
and :: Exp -> Exp -> Exp
and l r If l r false

-- disjunction (Or)
or::Exp -> Exp -> Exp
or l r = If l true r
-- or l r = If l l r
-- good idea but could be slow if L is large.

-- Example that uses synatical suger.


--uses all the syntatic sugar.
ex4 :: Exp

-- Syntactic suger is great untill we need to report errors because it makes for long trees of constructs that are not so simple.
-- We may pay some complexity cost in order to produce useful error messages.

-- *Statically type Stuff*























