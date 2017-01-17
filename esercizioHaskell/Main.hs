import Solutions
main = do
      let f= Imply (Var 'A') (And (Var 'A') (Var 'B'))
          x= vars f
          y= ttables x
          z=solutions f y
      putStrLn (show  x)
      putStrLn (show  y)
      putStrLn  (show  z)
