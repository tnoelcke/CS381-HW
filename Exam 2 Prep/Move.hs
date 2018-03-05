module Move where

type Pos = (Int, Int)

data Move = JumpTo Pos      -- move to that given position.
          | GoUp Int        -- move vertically
          | GoRight Int     -- move horizontally
          | Seq Move Move   -- do one move followed by another.