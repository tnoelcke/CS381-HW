module Move where

type Pos = (Int, Int)

data Move = JumpTo Pos      -- move to that given position.
          | GoUp Int        -- move vertically
          | GoRight Int     -- move horizontally
          | Seq Move Move   -- do one move followed by another.



sem :: Move -> Pos -> Pos
sem (JumpTo p) s = p
sem (GoUp i) (x, y) = (x, y + i)
sem (GoRight i) (x, y) = (x + i, y)
sem (Mov r l) p = sem l (sem r p)

