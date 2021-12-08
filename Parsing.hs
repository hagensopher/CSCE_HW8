--Functional parsing library from chapter 8 of Programming in Haskell,
--Graham Hutton, Cambridge University Press, 2007.
--
--Modified by Hyunyoung Lee as follows.
-- (1) Added arithmetic expressions at the end. 
-- (2) Fixed the problem of "missing superclasses of Monad" 
--     by making Parser also instance of Applicative and Functor.
-- (3) Instead of Parser being instance of MonadPlus, directly 
--     implemented the failure and choice (+++) parsers.
-- The modifications (2) and (3) are due to the AMP proposal that
-- was implemented in GHC version 7.10. 


module Parsing where

import Data.Char
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

infixr 5 +++

--The monad of parsers
----------------------

newtype Parser a              =  P (String -> [(a,String)])

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Functor Parser where
   fmap = liftM

instance Applicative Parser where
   pure = return 
   (<*>) = ap
 
--Basic parsers
---------------

failure                       :: Parser a
failure                       =  P (\inp -> [])

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

--Choice
--------

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

--Derived primitives
--------------------

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit, letter, alphanum       :: Parser Char
digit                         =  sat isDigit
letter                        =  sat isAlpha
alphanum                      =  sat isAlphaNum


lower, upper                  :: Parser Char
lower                         =  sat isLower
upper                         =  sat isUpper


char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p +++ return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  +++ nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()

--Ignoring spacing
------------------

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)



--Example: Arithmetic Expressions
---------------------------------
expr :: Parser Int
expr  = do t <- term
           do {char '+'
              ;e <- expr
              ;return (t + e)
              }
            +++ return t
           

term :: Parser Int
term  = do f <- factor
           do char '*'
              t <- term
              return (f * t)
            +++ return f

factor :: Parser Int
factor  = do d <- digit
             return (digitToInt d)
           +++ do char '('
                  e <- expr
                  char ')'
                  return e

eval   :: String -> Int
eval xs = fst (head (parse expr xs))


