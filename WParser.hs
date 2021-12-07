module WParser ( parse,
                 wprogram ) where

    import Data.Char
    import W

    import Control.Applicative (Applicative(..))
    import Control.Monad (liftM, ap)

    -----------------------------
    -- This is the main parser --
    -----------------------------
    wprogram = whitespace >> many stmt >>= \ss -> return (Block ss)
    -- a program is a sequence of statements; the parser returns them
    -- as a single block-statement

    -- only two of the statement types above are supported, the rest are undefined.
    -- please implement them
    stmt = varDeclStmt +++ assignStmt +++ ifStmt +++ whileStmt +++ 
           blockStmt +++ emptyStmt +++ printStmt

    emptyStmt = 
      symbol ";" >>
      return Empty
  
    printStmt = 
      keyword "print" >>
      expr >>= \e ->
      symbol ";" >>
      return (Print e)

    varDeclStmt = failure
    assignStmt = failure
    ifStmt = failure
    whileStmt = failure
    blockStmt = failure      

    -- the only kind of expression supported for now is stringLiterals
    -- implement the full expression language of W
    expr = stringLiteral 

    -- stringLiterals can contain \n characters
    stringLiteral = char ('"') >>
                    many stringChar >>= \s ->
                    char ('"') >>
                    whitespace >>
                    return (Val (VString s))

    stringChar = (char '\\' >> char 'n' >> return '\n') 
                 +++ sat (/= '"')

    ----------------------
    -- Parser utilities --
    ----------------------

    keywords = words "var if else while"
    isKeyword s = s `elem` keywords

    keyword s = 
      identifier >>= \s' ->
      if s' == s then return s else failure     
       
    newtype Parser a = P (String -> [(a, String)])
    
    parse :: Parser a -> String -> [(a, String)]
    parse (P p) inp = p inp
    
    instance Functor Parser where
        fmap = liftM
     
    instance Applicative Parser where
        pure  = return
        (<*>) = ap
    
    instance Monad Parser where
        -- return :: a -> Parser a
        return v = P $ \inp -> [(v, inp)]
                 
        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= q = P $ \inp -> case parse p inp of 
                                [] -> []
                                [(v, inp')] -> let q' = q v in parse q' inp'
    
    failure :: Parser a
    failure = P $ \_ -> []
    
    item :: Parser Char 
    item = P $ \inp -> case inp of 
                         (x:xs) -> [(x, xs)]
                         [] -> []
    
    -- Parse with p or q
    (+++) :: Parser a -> Parser a -> Parser a
    p +++ q = P $ \inp -> case parse p inp of 
                              [] -> parse q inp
                              [(v, inp')] -> [(v, inp')]
    
    
    -- Simple helper parsers
    sat :: (Char -> Bool) -> Parser Char
    sat pred = item >>= \c ->
               if pred c then return c else failure
    
    digit, letter, alphanum :: Parser Char
    digit = sat isDigit
    letter = sat isAlpha
    alphanum = sat isAlphaNum
    
    char :: Char -> Parser Char
    char x = sat (== x)
    
    string = sequence . map char 
    
    many1 :: Parser a -> Parser [a]
    many1 p = p >>= \v ->
              many p >>= \vs ->
              return (v:vs)
    
    many :: Parser a -> Parser [a]
    many p = many1 p +++ return []
    
    -- Useful building blocks
    nat :: Parser Int
    nat = many1 digit >>= \s ->
          whitespace >>
          return (read s)
    
    identifier :: Parser String
    identifier = letter >>= \s ->
                 many alphanum >>= \ss ->
                 whitespace >>
                 return (s:ss)
    
    whitespace :: Parser ()
    whitespace = many (sat isSpace) >> comment
    
    symbol s = 
        string s >>= \s' ->
        whitespace >>
        return s'    
    
    comment = ( string "//" >>
                many (sat (/= '\n')) >>
                whitespace ) +++ return ()
    
    parens p = 
        symbol "(" >> 
        p >>= \res ->
        symbol ")" >>
        return res
