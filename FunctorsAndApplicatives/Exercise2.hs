--
-- Author: Mirko Bez
--
-- Exercise 2
--
listOfThreeElements l1 l2 l3 = [x:y:z:[] | x <- l1, y <- l2, z <- l3]

foo1 l1 l2 l3 = [x:y:z:[] | (x,y,z) <- zip3 l1 l2 l3]

bar_rec [] _ = []
bar_rec (xs:xss) n = map (\x -> x + n) xs : (bar_rec xss (n+1))

bar xs = bar_rec xs 1

