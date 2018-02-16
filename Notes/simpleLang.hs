--- | a single register imperative language.
module While where

-- *syntax
--
--
-- int ::= (any int)

--
--
-- expr ::= `R`                      load form reg
--            | int                        int literal
--             | expr + expr         Addtion
--
-- test := expr <= expr         less than or equal
--
--
-- stmt ::= R = expr               Set Register
--          | while test stmt        While loop
--           | begin stmt* end    statement block


data Expr = Get
                | Lit Int
                | Add Expr Expr
                deriving(Eq, Show)
                
data test = LTE expr expr
               deriving(Eq, Show)
               
data Stmt = Set Expr 
                | While Test Stmt
                | Begin [Stmt]
         deriving(Eq,Show)
                
                
 -- example program
 -- begin R:= 1
 --while R <= 100
 --     R = R + R
 -- end
 
 p :: Stmt
 p = Begin [
                  Set (Lit 1), 
                  While(LTE Get (Lit 100)),
                        (Set (Add Get Get)),
                  ]
  

-- *semanatics

-- | current value of the register.
Type Reg = Int


-- | valuation functinofor 
-- if something could go wrong we would insert a maybe Int
expr :: -> Expr -> Reg -> Int 
expr Get           = \reg -> r
expr (Lit i)        =  \reg -> i
exp (Add l r)     =  \reg -> expr l reg + expr r reg

--valuation function for tests
test :: Test -> Reg ->Bool
test (LTE l r) = \reg -> expr l reg <= expr r reg


-- | Non-compositional valuation function for statement
-- this works for a interpreter. 
-- semantics is all about mapping terms to meaning. 
-- This won't mathmatically represent our language.
stmt -> Stmt -> Reg -> Reg
stmt (Set e)         = \reg -> expr e reg
stmt (While t s)    = \reg -> if test t reg
                                         then stmt (While t s) (stmt s reg)
                                         else reg
stmt (Begin ss)    = \reg ->  foldl (flip stmt) reg ss


-- need definition for foldl








 
 
 
 
 