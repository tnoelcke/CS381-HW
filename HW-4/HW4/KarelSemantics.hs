module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing d) w r =  d == (getFacing r)
test (Clear d) w r =  isClear (neighbor (cardTurn d (getFacing r)) (getPos r)) w
test (Beeper) w r = hasBeeper (getPos r)  w
test (Empty) w r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w r = if (test (Clear Front) w r)  then OK w (updatePos (neighbor (getFacing r)) r)
                                               else Error ("Blocked at:" ++ show (neighbor (getFacing r) (getPos r)))
stmt PutBeeper _ w r = if isEmpty r then Error("No beeper to put.") 
                                    else OK (incBeeper (getPos r) w) (decBag r)
stmt (Turn d) _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)  
stmt (Call m) d w r = case lookup m d of
                        Just s -> stmt s d w r
                        _-> Error("Undefined macro: " ++ m)
stmt (Iterate i s) d w r =  case stmt s d w r of 
                            Done r' -> stmt (Iterate (i - 1) s) d w r'
                            OK w' r' -> stmt (Iterate (i -1) s) d w' r' 
                            Error e -> Error e
stmt (If t s1 s2) d w r = if (test t w r) then stmt s1 d w r 
                                      else stmt s2 d w r
stmt (While t s) d w r = if (test t w r) then case stmt s d w r of
                                               Done r' -> stmt (While t s) d w r'
                                               OK w' r' -> stmt (While t s) d w' r'
                                               Error e -> Error e
                                     else OK w r
stmt (Block (s:xs)) d w r = case stmt s d w r of
                                    Done r' -> stmt (Block xs) d w r'
                                    OK w' r' -> stmt (Block xs) d w' r'
                                    Error s -> Error s
stmt (Block []) d w r = OK w r
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
