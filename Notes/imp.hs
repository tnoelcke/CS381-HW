module Imp where




-- | Abstract syntax of Imp lang.
data Expr = Lit Int
                | Add Expr Expr
                | LTE Expr Expr
                | Not Expr
                | Ref Var
               deriving(show, Eq)
 
 data stmt = Bind Var Expr
                   | If Expr Stmt Stmt
                   | While Expr Stmt
                   | Block [Stmt]
              deriving(show, Eq)
              
 
 
 -- Abstract Syntax of types
 
 data Type = TInt | TBool
 
 type Decl = (Var, Type)

 data Prog = P [Decl] Stmt 
 
 -- | Absract Syntax of programs
 
 -- prog ::= decl* 'begin' stmt
 
 -- Also OK 
 -- type Prog = ([Decl], stmt)
 
 ex1 :: Prog
 ex1 = P [("sum", TInt), ("n", TInt)] 
              (Block [ 
                    Bind "sum" (Lit 0),
                    Bind "n"     (Lit 1),
                    While (LTE (Ref "n") (Lit 100)),
                    (Block [
                        Bind "sum" (Add (Ref "sum") (Ref "n")),
                        Bind "n"     (Add (Ref "n") (Lit 1))
                    ])
              ])


              
              
              
-- typeOf :: Expr -> Maybe Type
-- Need to see what types names are mapped to.
-- We can also have undeclaired variable error.

-- typeOf :: Expr -> Env Type -> Maybe Type

ex2:: Prog
ex2 = P[("x", TInt)]
            (Bind "x" (LTE (Lit 3) (Lit 4)))
            
--variable environment
type Env a = Map Var a



