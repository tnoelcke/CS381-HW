module MiniLogo where

import Prelude hiding (Num)
--
-- * MiniLog
-- Sutdents in Group:
-- ONID ID		Full Name
-- -------      -------------------
-- noelcket		Thomas Noelcke

-- Here are an outline of tasks that need to be completed.
-- View the outline of the assignment for the syntax of the
-- MiniLog language.

-- 1.Define the abstract syntax of minilog as a set of haskell data types.
--you should use built-in types for num, var, and macro. (if you want to
--define a type Num you will ahve vto hide the name from the Prelude).




-- num ::-= (any natural number)
-- var ::= (Any Variable name)
-- macro ::= (Any String name)

type Num = Int
type Var = String
type Macro = String

--Prog = element | cmd; prog	Sequence of commands

type Prog = [Cmd]

-- mode ::= down | up	Pen status

data Mode = Down | Up
          deriving(Show, Eq)  

-- expr ::= var				variable ref
--		|	num				literal number
--		|	expr + expr		addition expression


data Expr = Var Var
          | Num Num
          | Add Expr Expr
          deriving(Eq, Show)

-- cmd ::=  pen mode					Change pen mode
--		|	move (expr, expr)			move pen to new position		  
--		|	define macro (var*) {prog}	Define a macro
--		|	call macro ( expr*) 		invoke a macro

data Cmd = Pen Mode
        |  Move Expr Expr
        |  Define Macro [Var] Prog
        |  Call Macro [Expr]
        deriving(Eq, Show)


-- 2. Define a MiniLogo macro line (x1, y1, x2, y2) that draws a line.
line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] 
        [Pen Up,   Move (Var "x1") (Var "y1"), 
        Pen Down, Move (Var "x2") (Var "y2")] 


-- 3. use the line macro you just defined a new minilogo macro nix (x, y, w, h)
-- this function will draw a big X from the origin (x, y) your definition
-- should not contain any move commands.
nix :: Cmd
nix = Define "nix" 


-- 4. Define a haskell function steps :: Int -> Prog that constructs
-- a MiniLogo program that draws a staircase of n steps starting form (0, 0).

-- steps :: Int -> Prog

-- 5. Define a Haskell function macros that returns a list of the names of all the
-- macros that are defined anywhere in a given MiniLogo program. Don't worry about duplicates.

-- macros :: Prog -> [Macro]


-- 6. Define a haskell function pretty that pretty prints a MiniLogo program. that is it
-- transforms the abstract syntax into nicely formatted concrete syntax (as string of characters). 
-- These should look like the examples in the HW write up.

-- pretty -> Prog -> String

-- BOUNUS PROBELMS

-- 7. Define a Haskell function optE that partially evaluates expressions by replacing
-- any additions of literals with the result.

-- optE -> Expr -> Expr

-- 8. Define a Haskelll function optP that optimizes all of the expressions contained in a given
-- program using optE.

-- optP :: Prog -> Prog

