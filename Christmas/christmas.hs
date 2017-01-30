import Control.Applicative
import Data.Char

-----------------------------------------------------------------
--
-----------------------------------------------------------------

---------------------------------------------------
-- Library of parser of the class
---------------------------------------------------
		
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of 
					[] -> []
					(x:xs) -> [(x,xs)])

instance Functor Parser where
	--fmap :: (a -> b) -> Parser a -> Parser b
	fmap g p = P (\inp -> case parse p inp of
					[] -> []
					[(v,out)] -> [(g v, out)])
-- End of Functor Parser;				


instance Applicative Parser where
	-- pure :: a -> Parser a
	pure v = P (\inp -> [(v, inp)])
	
	-- <*> :: Parser (a -> b) -> Parser a -> Parser b
	pg <*> px = P (\inp -> case parse pg inp of 
						[] -> []
						[(g, out)] -> parse (fmap g px) out)
-- End of Applicative Parser;				

instance Monad Parser where
	-- 
	return = pure
	-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
	p >>= f = P (\inp -> case parse p inp of 
				[] -> []
				[(v, out)] -> parse (f v) out)
--End of Monad Parser;

instance Alternative Parser where
	-- empty :: Parser a
	empty = P (\inp -> [])
	
	-- (<|>) :: Parser a -> Parser a -> Parser a
	p <|> q = P (\inp -> case parse p inp of 
					[] -> parse q inp
					[(v, out)] -> [(v, out)])
--End Alternative Parser

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x 
		         then return x 
		         else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)



                   
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser()
space = do many (sat isSpace)
           return ()


int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

list :: Parser a -> Parser [a]
list p = do symbol "["
            n <- p
            ns <- many (do symbol ","
                           p)
            symbol "]"
            return (n:ns)


nats :: Parser [Int]
nats = list natural

integers :: Parser[Int]
integers = list integer



---------------------------------
-- Exercise 1
---------------------------------

                                    

comment :: Parser()
comment = do symbol "--"
             many (sat (/= '\n')) --Eventually one can also read the '\n' 
             return ()
                
                   
---------------------------
-- Exercise 2
---------------------------
-- expr ::= expr + expr | expr * expr | ( expr ) | nat

data Expr = NAT Int | ADD Expr Expr | SUB Expr Expr | MULT Expr Expr | DIV Expr Expr deriving (Show, Eq)


expr :: Parser Expr
expr = do t <- term
          do symbol "+"
             e <- expr
             return (ADD t e)
             <|>
             do symbol "-"
                e <- expr
                return (SUB t e)
             <|> return t


term :: Parser Expr
term = do f <- factor
          do symbol "*"
             t <- term
             return (MULT f t)
             <|>
             do symbol "/"
                t <- term
                return (DIV f t)
             <|>
             return f

factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|>
         do n <- natural
            return (NAT n)

------------------------------
-- Exercise 3
------------------------------

---- Part a: Describe The Grammar
---- BNF Grammar

---- expr   ::= term + expr | expr - term | term
---- term   ::= factor * term |  factor
---- factor ::= (expr) | nat
---- nat    ::= 0 | 1 | 2 | ...


---- Part b: implement this grammar as a parser expr:: Parser Int.

expr3 :: Parser Int
expr3 = do t <- term3
           do symbol "+"
              e <-expr3
              return (t + e)
           <|>
           do e <- expr3
              do symbol "-"
                 t <- term3
                 return (e - t)
           <|>
             term3
     

term3 :: Parser Int
term3 = do f <- factor3
           do symbol "*"
              t <- term3
              return (f * t)
              <|> return f

factor3 :: Parser Int
factor3 = do symbol "("
             e <- expr3
             symbol ")"
             return e
          <|> natural

---- Part c: The problem with this parser is that it is stuck
---- when we use expr3. This is because when we fail to read a term
---- then we try to read an expr (without consuming) input and this
---- leads to a recursive call that does not consume inputs

--workingExpr3 :: Parser Int

expr3' :: Parser Int
expr3' = do t <- term3'
            do symbol "+"
               e <-expr3'
               return (t + e)
            <|>
            do t <- expr3'
               do symbol "-"
                  t <- term3'
                  return (e - t)
            <|>
            term3'                
term3' = do f <- factor3'
            do symbol "*"
               t <- term3'
               return (f * t)
               <|> return f                              

factor3' :: Parser Int
factor3' = do symbol "("
              e <- expr3'
              symbol ")"
              return e
           <|> natural
 
----------------------------------
-- Exercise 4
----------------------------------
--fibs :: [Integer]

--nextFib = (\(a,b) -> (b, a+b))
--start = [(0,1)]
--fib_help :: [(Integer, Integer)] -> [(Integer, Integer)]
--fib_help (x:xs) = x : fib_help (nextFib x:[])

--fibs = map fst (fib_help start)



fibs :: [Integer]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]
