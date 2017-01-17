--Author: Mirko Bez
--Description: First Haskell assignment 
module Solutions(vars, ttables,solutions, Prop(..)) where

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop

-- Data una formula proposizionale F, rappresentata da un valore del tipo Prop deve
--restituire la lista delle variabili proposizionali distinte che sono contenute in F.

-- Get all the variables (with repetition)
vars_rec :: Prop ->[Char]
vars_rec (Const _) 		= []
vars_rec (Var a)		= [a]
vars_rec (Not p) 		= vars_rec p
vars_rec (And p q)	 	= vars_rec p ++ vars_rec q
vars_rec (Imply p q) 	        = vars_rec p ++ vars_rec q 

--Removing repetitions and preserving the order
vars :: Prop ->[Char]
vars p = foldl (\x y -> if y `elem` x then x else x ++ [y]) [] (vars_rec (p))

--Data una lista L di variabili proposizionali produce la lista di tutte le
--sostituzioni di valori di verità True/False per le variabili di L. Quindi per ['A','B'], ttables produce [[('A',True),
--('B',True)], [('A',True),('B',False)], [('A',False),('B',True)], [('A',False),('B',False]]. Ogni sottolista è
--un'assegnazione di valori di verità per tutte le variabili della formula, e tutte le possibili assegnazioni sono
--prodotte.
ttables ::[Char] -> [[(Char, Bool)]]
ttables l = foldr(\y previous_res -> [(y, x):xs | xs <- previous_res, x <- [True, False]]) [[]] l

-- Recursive definition without using foldr
-- ttables [] = [[]]
-- ttables (y:ys) = [(y,x):xs | xs <- ttables ys, x <- [True,False]]

--solutions :: Prop -> [[(Char,Bool)]]-> Bool considera ogni assegnazione di valori di verità per le variabili della
--formula (primo parametro) e valuta se la formula è vera o no per quell'assegnazione. Deve compiere questa
--operazione per ogni assegnazione e restituisce True sse la formula è sempre vera, cioè sse è una tautologia.
ev_rec :: Prop -> [(Char, Bool)] -> Bool
ev_rec (Const a) 	_	= a
ev_rec (Var a)		tt	= case lookup a tt  of Just v -> v 
ev_rec (Not p) 		tt	= not (ev_rec p tt)
ev_rec (And p q) 	tt	= (ev_rec p tt) && (ev_rec q tt)
ev_rec (Imply p q) 	tt	= not(ev_rec p tt) ||  (ev_rec q tt) 

solutions :: Prop -> [[(Char, Bool)]] -> Bool
solutions p l = and (map (\x -> ev_rec p x)  l)

--A more efficient solution (stops if an intermediary result is False. May be useful with big tables --
solutions' p [] = True
solutions' p (x:xs) = if ev_rec p x then solutions' p xs else False 



