import System.IO
-- Mirko Bez
-- File: hangman.hs



strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

hangman :: IO ()
hangman = do putStrLn "Think a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             play word


play :: String -> IO()
play word = do putChar '?'
               xs <- getLine
               if xs == word
                  then
                  do putStrLn "You guess it"
                     return ()
               else
                  do putStrLn (map (\x -> if x `elem` xs then x else '-') word)
                     play word
            







sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                  do putChar '-'
                     xs <- sgetLine
                     return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

