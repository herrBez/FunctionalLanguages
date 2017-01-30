import Control.Applicative
----------------------
-- Author: Mirko Bez
-- 
----------------------

  

---------------------------------------------------------------------
-- Exercise (1)
-- This exercise is Commented out because otherwise the rest does not
-- compile
---------------------------------------------------------------------

--
-- Define an instance of the Monad class for the type (a ->).
-- Remember that one has to write, instance
-- Monad ((->)a) where...

-- RECAP a -> b can be rewritten as (->) a b

--instance Functor ((->) a) where
-- --fmap :: (b->c) -> (a->b) -> a -> c
--  fmap = (.)

--instance Applicative ((->) a) where
--  --pure :: b -> (a -> b)
--  pure x = (\_ -> x)

-- -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a-> c)
--    g <*> h = \x -> g x (h x)
  
--instance Monad ((->) a) where
--   -- return : b -> (a -> b)
--   return = pure
--   -- (>>=) :: (a -> b) -> (b -> a -> c) -> (a -> c)
--   h >>= f = \w -> f (h w) w

---------------
-- Exercise (2)
---------------
--Given the following type of expressions
--that contains variables of some tipe a, show how to make this
--type into instances of Functor, Applicative and Monad classes.
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
	--fmap :: (a->b) -> Expr a -> Expr a
	fmap f (Var a) = Var (f a)
	fmap _ (Val i) = Val i
	fmap f (Add b c) = Add (fmap f b) (fmap f c) 

instance Applicative Expr where
	-- pure :: a -> f a
	pure = Var 
	-- <*> :: Expr (a -> b) -> Expr a -> Expr b
	(Var g) <*> e = fmap g e
        (Val n) <*> e = Val n
	(Add g h) <*> e = Add (g <*> e) (h <*> e)

instance Monad Expr where
	-- return :: Applicative f => a -> f a
	return = pure
	-- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
	(Val x) >>= _ = (Val x)
	(Var x) >>= f = f x
	(Add x y) >>= f = (Add (x >>= f) (y >>= f))



--With the aid of an example, explain what the >>= operator for this type does

-- The (>>=) operator apply the given function to each of the variable
-- without affecting the values, that remains always integer

-- Little Example: Assume that the variable are in the range [1..26]
-- I define the lookup table intToAlphaMap that is composed by 
-- the tuples (i, a) where i is the position of the alphabet (only
-- lower case, for simplicity) of the letter a. 
intToAlphaMap = zip [1..] ['a'..'z']

-- This function maps a position to the corresponding letter
toAlpha k = case [v | (k', v) <- intToAlphaMap, k == k'] of 
		[x] -> x
-- This function map a letter to the corresponding position
toInt a = case [i | (i, a') <- intToAlphaMap, a == a'] of
		[x] -> x
 
-- fun :: Applicative f => Integer -> Expr Char
fun x = pure (toInt x)


ex = (Add (Add (Var 12) (Var 26)) (Val 11)) >>= (\x -> pure (toAlpha x))
	

---------------
-- Exercise (3)
---------------
-----------------------------------------------------------------------
--Rather than making a parameterized type into instances of the Functor
-- , Applicative and Monad classes in
--this order, in practice it is sometimes simpler to define the Functor and Applicative instances in terms of
--the Monad instance, relying on the fact that the order in which the declarations are made is not important
--in Haskell. Thus, given the monad definition
------------------------------------------------------------------------



type State = [Char]

newtype ST a = S (State -> (a, State)) 

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
	--fmap :: (a -> b) -> ST a -> ST b
	fmap g st = 
		do 	x <- st
			return (g x)
			

instance Applicative ST where
	--pure :: a -> ST a
	pure x = S (\s -> (x,s))
	-- (<*>) :: ST (a -> b) -> ST a -> ST b
	stf <*> stx = do
		f <- stf
		x <- stx
		return (f x)


instance Monad ST where
	return x = pure x
	-- (>>=) :: ST a -> (a -> ST b) -> ST b
	st >>= f = S (\s -> 
		let (x, s') = app st s in app (f x) s')


