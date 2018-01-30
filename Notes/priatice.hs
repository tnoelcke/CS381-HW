import Prelude hiding (Num)

--
-- Syntax of StackLang
--
-- RPM (Reverse polish notation)
-- (3+4) * 5 -> 34+5*
-- 3 * (4 + 5) -> 3 4 5 + *
-- 
-- Grammar for StackLang:
--
--
-- num ::= (any integer)
-- bool ::= `true` | `false` 
-- prog ::= cmd*							push a number on the stack
-- 		|	bool							push boolean on the stack
--		|	`add`							add two numebrs on the stack
--		|	`eq`							check wetaher the top two elements are equal
--		|	`if` prog	`else` prog `end`	if its true run the first program, else run the second 

--1. encode the above grammar as a set of haskell data types
type Num = Int
-- Bool = Bool

-- program is a list of commands
type Prog = [Cmd]

type Cmd = PushN Num
         | PushB Bool
		 | Add
		 | Equ
		 | IfThenElse Prog Prog
    deriving(Eq, Show)

--2. Write the following StackLang program as a Heskell valuex
-- 3 4 add 5 Eq
ex1 :: Prog
ex1 = [PushN 3, PushN, 4, Add, PushN 5, Equ]

-- 3. Write a StackLang program tha:
-- * cheks weather 3 and 4 are equal
-- * if so returns the reuslt of add 5 6 
-- else reutrns false
--
-- 3 4 equ if 5 6 add else false end
ex2 -> Prog
ex2 = [PushN 3, PushN 4, Equ, IfThenElse [PushN 5, PushN 6, Add][PushB false]]

--4. write a haskell function that takes x and y and generates a stacklange program that adds both x and y to the number at the top of the stack.
getAdd2 -> Num -> Num -> Prog
genAdd2 x y = [PushN x, PushN y, Add, Add]


--5. write a haskell function that takes a list of Integers and takes a StackLang program that sums them all up.
genSum = undefined