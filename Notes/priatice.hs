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

-- Continued on 1/31/2018
-- Notes Continuing from last lecture.
-- HW Info:
-- Pretty printing to concrete syntax should traverse abstract syntax Should output
-- concrete syntax with pluses instead of add.

--5. write a haskell function that takes a list of Integers and takes a StackLang program that sums them all up.
genSum :: [Int] -> Prog
genSum [] = [PushN 0] 
genSum (x:xs) = genSum x ++ [PushN x, Add]



-- genSum (x:xs) = [PushN x, Add] ++ genSum xs
-- good idea but not quite. It doesn't generate the correct commands
-- [PushN 1, Add] -- NO GOOD!
-- genSum[] = [PushN 0] !Same error change recursion order.




-- genSum (x:xs) = [PushN x, Add, genSum x]
-- Good idea but type error


--Semantics: Will do later!

-- 2 - 12 - 2018 Notes

-- Reviewed stack lang language.
--6. Identify/define the semantics domain for the Cmd and for Prog
-- things we can encounter:
--      * Numbers
--      * Stack
--      * Boolean
-- * Errors 
--     * type error
--     * stack underflow error
-- data Either a b  = Left a | Right b


-- stack
type Stack = [Either Num Bool]

-- stigmatics of a command
type Domain = Stack -> Maybe Stack

--7. Define the semantics of StackLang command (ignore If at first)

cmd :: Cmd -> Stack -> Maybe Stack
cmd (PushN n)          = \s -> Just  (Left n : s)
cmd (PushB n)          = \s -> Just (Right n : s )
cmd Add                  = \s  -> case s of
                                                (Left n : Left m: s')  -> Just (Left (n+m)  : s')
                                                _ -> Nothing
cmd Equ                  = \s -> case s of
                                                (Left n : Left m : s') -> Just (Right (n == m) : s')
                                                (Right b : Right c : s') -> Just (Right( b == c) : s')
                                                _ -> Nothing
cmd (IfThenElse t e) = \s -> case s of
                                                (Right True : s')  -> prog t s'
                                                (False False : s') -> prog e s'
                                                _ -> Nothing




-- 8. Define the semantics of a StackLang program

prog :: Prog -> Stack -> Maybe Stack
prog []  = \s -> Just s
prog (c:cs) = \s -> case  cmd c s  of
                                Just s'   -> prog cs s'  
                                Nothing -> Nothing











