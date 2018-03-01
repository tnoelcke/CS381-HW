data Exp = Lit Int
                | Add Exp Exp
                | Let Var Exp Exp
                | Ref Var
                | Fun Var Exp
                | App Exp Exp
             deriving(Eq, Show)
             
-- | ** example programs. Look at slides and online examples because i don't want to type this.
type Var = String

type Env a = [(Var, a)]

data DVal = DI Int              --integer
                  | DF Var Exp     -- Function
                  
dsem :: Exp ->  Env DVal  -> Maybe DVal
dsem (Lit i)        m = Just (DI i)
dsem (Add l r)   m = case (dsem l m, dsem r m) of
        