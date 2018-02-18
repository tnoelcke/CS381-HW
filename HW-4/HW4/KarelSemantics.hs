module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing d) w r =  Eq d (getFacing r)
test (Clear d) w r = undefined
test (Beeper) w r = hasBeeper (getPos r)  w
test (Empty) w r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w r = undefined
stmt PutBeeper _ w r = undefined
stmt (Turn d) _ w r = undefined
stmt (Call m) Defs w r = undefined
stmt (Iterate i s) Defs w r = undefined
stmt (If t s s) Defs w r = undefined
stmt (While t s) Defs w r = undefined
stmt (Block (s:xs)) Defs w r = undefined 
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
