--module BezParser where

import Control.Applicative
import Data.Char

-----------------------------------------------------------------
--
-----------------------------------------------------------------


-- Words that cannot be used as variables/function name
reservedKeywords = ["if", "then", "else", "lambda", "and", "let", "letrec", "in", "end",
                    "null", "cons", "tail", "eq", "leq"] 

data LKC = VAR String | NUM Int | NULL | ADD LKC LKC |
			SUB LKC LKC | MULT LKC LKC | DIV LKC LKC |
			EQ LKC LKC | LEQ LKC LKC | H LKC | T LKC | CONS LKC LKC |
			IF LKC LKC LKC | LAMBDA [LKC] LKC | CALL LKC [LKC] |
			LET LKC [(LKC,LKC)] | LETREC LKC [(LKC, LKC)]
			deriving(Show, Eq)


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


------------------------------------
--- BEGIN OF THE LISP KIT PARSER ---
------------------------------------

var_help :: Parser String
var_help = do x <- letter
              xs <- many alphanum
              let l = (x:xs)
              if(elem l reservedKeywords) --Checking if the var is a keyword
				then 
					empty
				else
					return (x:xs)

var :: Parser String
var = token var_help



factor :: Parser LKC
factor  = do s <- var
             symbol "("
             n <- expr
             l <- many (do symbol ","
                           expr)
             symbol ")"
             return (CALL (VAR s) (n:l))
          <|>
          do s <- var
             symbol "("
             symbol ")"
             return (CALL (VAR s) [])
          <|>
          do s <- var
             return (VAR s)
          <|>
          do symbol "("
             e <- expa
             symbol ")"
             return e
          <|>
          do n <- integer
             return (NUM n)
          <|>
          do symbol "null"
             return (NULL)
          

term :: Parser LKC
term = do f <- factor
          symbol "*"
          t <- term
          return (MULT f t)
       <|>
       do f <- factor
          symbol "/"
          t <- term
          return (DIV f t)
       <|>
       do factor

expa :: Parser LKC
expa = do t <- term
          symbol "+"
          e <- expa
          return (ADD t e)
       <|>
       do t <- term
          symbol "-"
          e <- expa
          return (SUB t e)
       <|>
       do term


opp_unary :: (LKC -> LKC) -> String -> Parser LKC
opp_unary c token = do 
                      symbol token 
                      symbol "("
                      e <- expr
                      symbol ")"
                      return (c e)

p_tail = opp_unary (T) "tail"
p_head = opp_unary (H) "head"

opp_binary :: (LKC -> LKC -> LKC) -> String -> Parser LKC
opp_binary c token = do
                       symbol token
                       symbol "("
                       e1 <- expr
                       symbol ","
                       e2 <- expr
                       symbol ")"
                       return (c e1 e2)

p_cons = opp_binary (CONS) "cons"
p_eq = opp_binary (Main.EQ) "eq"
p_leq = opp_binary (LEQ) "leq"

opp = p_head <|> p_tail <|> p_cons <|> p_eq <|> p_leq


varCons = do v <- var
             return (VAR v)


lambda = do symbol "lambda"
            symbol "("
            l <- many varCons
            symbol ")"
            e <- expr
            return (LAMBDA l e)
         
-- if e1 then e2 else e3
ifthenelse :: Parser LKC
ifthenelse = do symbol "if"
                e1 <- expr
                symbol "then"
                e2 <- expr
                symbol "else"
                e3 <- expr
                return (IF e1 e2 e3)

bind :: Parser (LKC, LKC)
bind = do v <- var
          symbol "="
          e1 <- expr
          return (VAR v, e1)

binds :: Parser [(LKC, LKC)]
binds = do b <- bind
           l <- many (do symbol "and"
                         bind)
           return (b:l)

-- The order is important !!
expr = prog <|> lambda <|> opp  <|> ifthenelse <|> expa 



-- Prog Parser Section --
p_let_and_letrec c token =  do symbol token
                               b <- binds
                               symbol "in"
                               e <- expr
                               symbol "end"
                               return (c e b)


p_let = p_let_and_letrec LET "let"
p_letrec = p_let_and_letrec LETREC "letrec"
prog = p_let <|> p_letrec

---------------------
-- Test Cases
---------------------

test1 :: String
test1 = "let x = 2 and y = 4 in x+y*2 end"

test2 :: String
test2 = "letrec fact = lambda(n) if eq(n,1) then 1 else n * fact(n-1) and x=cons(1, cons(2, null)) and f = lambda (l g) if eq(l,null) then null else cons (g (head(l)), f(g, tail(l))) in f(x, fact) end" 

