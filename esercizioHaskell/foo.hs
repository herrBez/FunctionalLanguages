
truth 0 = [[]]
truth n = [x:xs | xs <- truth (n-1), x <- [True, False]] 

ttables :: [Char] -> [[(Char, Bool)]]
ttables [] = [[]]
ttables (y:ys) = [(y,x):xs | xs <- ttables ys, x <- [True,False]]



