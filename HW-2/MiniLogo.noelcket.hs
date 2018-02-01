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


-- Use the line you just defined to define a new MiniLogo macro nix(x, y, w, h)
-- That draws a big "X" if width w and height h starting from position
--(x,y). your definition should not contain any move commands.
--  -   Write the macro in minilogo concrete syntax and include this definition in
--      In a comment in your submission. 
--
--  Define Macro nix (x, y, w, h) {
--      call line(x, y, x + w, y + h);
--      call line(x + w, y, x, y + h); 
--  }
--
--  -   encode the marcro definition as  a Haskell value representing the abstract syntax of the definition.

nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
                   [Call "line" [Var "x", Var "y", Add (Var "x") (Var "w"), Add (Var "y") (Var "h")],
                    Call "line" [Var "x", Add (Var "y") (Var "h"), Add (Var "x") (Var "w"), Var "y"]]


-- 4. Define a haskell function steps :: Int -> Prog that constructs
-- a MiniLogo program that draws a staircase of n steps starting form (0, 0).

-- steps :: Int -> Prog

steps :: Int -> Prog
steps 1 = [Pen Up, Move (Num 0) (Num 0),
           Pen Down, Move (Num 0) (Num 1), Move (Num 1) (Num 1)]
steps i = steps (i - 1) ++ [Move (Num (i - 1)) (Num i), Move (Num i) (Num i)] 


-- 5. Define a Haskell function macros that returns a list of the names of all the
-- macros that are defined anywhere in a given MiniLogo program. Don't worry about duplicates.

macros :: Prog -> [Macro]
macros [] = []
macros ((Define mac _ _) : c) = mac : macros c
macros ((Pen _) : c )           = macros c
macros ((Move _ _): c )         = macros c
macros ((Call _ _): c )         = macros c

-- 6. Define a haskell function pretty that pretty prints a MiniLogo program. that is it
-- transforms the abstract syntax into nicely formatted concrete syntax (as string of characters). 
-- These should look like the examples in the HW write up.

pretty :: Prog -> String
Pretty []= ""
Pretty ((Pen p):c = "pen " ++ (case p of
                                Up -> "up; "
                                Down -> "down; ") ++ pretty c  


