import Control.Applicative
type Stack = [Char]

-- pop :: Stack -> (Char, Stack)
pop (x:xs) = (x, xs)
-- push :: Char -> 	Stack -> ((), Stack)
push x xs = ((), x:xs)
-- check_par : takes a string and a Stack and says
-- True/False when the string is/is not correctly b
-- Balanced.
-- check_par :: [Char] -> Stack -> Bool
check_par [] xs = null xs
--check_par (')':string) [] = False -- Trying to pop from empty Stack
check_par ('(':string) xs = let (_, stack) = push ')' xs in check_par string stack
check_par (')':string) xs = let (_, stack) = pop xs in check_par string stack 
check_par (_:string) xs = check_par string xs

---------------------------------------------------
--- Solution
---------------------------------------------------



type Stato = [Char]
newtype ST a = S(Stato -> (a, Stato))

--app :: ST a -> Stato -> (a, Stato)
app (S st) x = st x

instance Functor ST where
	--fmap :: (a->b) -> ST a -> ST b
	fmap g st = S (\s -> let(x,s') = app st s in (g x, s'))

instance Applicative ST where
	-- pure :: a -> ST a
	pure x = S (\s -> (x,s))
	
	-- (<*>) :: ST (a -> b) -> ST a -> ST b
	stf <*> stx = S (\s -> 
		let
		(f, s') = app stf s
		(x, s'') = app stx s' in (f x, s''))


instance Monad ST where
	return = pure
	-- (>>=) :: ST a -> (a -> ST b) -> ST b
	st >>= f = S (\s -> 
		let (x, s') = app st s in app (f x) s')


--------------------
-- TODO use push1
--------------------

pop1 :: ST Char
pop1 = S (\(s:ss) -> (s, ss))

push1 :: ST()
push1 = S(\s -> ((), ')':s))



-- empty :: ST Bool
stackEmpty = S (\s -> ((null s), s))



check_par1 :: [Char] -> ST Bool
check_par1 [] = do
                   e <- stackEmpty
	           return e
check_par1 ('(':xs) = do
                        push1
                        check_par1 xs
check_par1 (')':xs) = do
			pop1
                        check_par1 xs
check_par1 (x:xs) = check_par1 xs
						
						
				  



			   
	


