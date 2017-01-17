module Solutions(vars, ttables,solutions, Prop(..))

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop

-- Data una formula proposizionale F, rappresentata da un valore del tipo Prop deve
--restituire la lista delle variabili proposizionali distinte che sono contenute in F.
vars_rec :: Prop ->[Char]
vars_rec (Const _) 		= []
vars_rec (Var a)		= [a]
vars_rec (Not p) 		= vars_rec p
vars_rec (And p q)	 	= vars_rec p ++ vars_rec q
vars_rec (Imply p q) 	= vars_rec p ++ vars_rec q 

vars :: Prop ->[Char]
vars p = foldl (\x y -> if y `elem` x then x else x ++ [y]) [] (vars_rec (p))

--Data una lista L di variabili proposizionali produce la lista di tutte le
--sostituzioni di valori di verità True/False per le variabili di L. Quindi per ['A','B'], ttables produce [[('A',True),
--('B',True)], [('A',True),('B',False)], [('A',False),('B',True)], [('A',False),('B',False]]. Ogni sottolista è
--un'assegnazione di valori di verità per tutte le variabili della formula, e tutte le possibili assegnazioni sono
--prodotte.
ttables::[Char] -> [[(Char, Bool)]]
ttables l = sequence (map (\x -> (x, True):(x,False):[]) l)

--solutions :: Prop -> [[(Char,Bool)]]-> Bool considera ogni assegnazione di valori di verità per le variabili della
--formula (primo parametro) e valuta se la formula è vera o no per quell'assegnazione. Deve compiere questa
--operazione per ogni assegnazione e restituisce True sse la formula è sempre vera, cioè sse è una tautologia.
solutions :: Prop -> [[(Char, Bool)]] -> Bool
solutions p l = True


