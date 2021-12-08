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
      keyword "print" >> --looks for the keyword "print"
      expr >>= \e -> --makes an expression and result into e then
      symbol ";" >> -- find the symbol ";"
      return (Print e) --print it

    varDeclStmt = 
      keyword "var" >>
      whitespace >> 
       many alphanum >>= \f -> --what i think: this checks if the next after var is a letter
      whitespace >>
      symbol "=" >> -- checks if the value after the letter is an equals sign
      expr >>= \e ->
      symbol ";" >>
      return (if not (isKeyword f) then (VarDecl f e) else undefined ) --print it
      
    assignStmt = 
      identifier >>= \f ->
      whitespace >>
      symbol "=">>
      expr >>= \e ->
      symbol ";" >>
      return (Assign f e)
-- a very strict defenition of if
  -- problems 
  --1. no partheneis 
  --2. cant handle multiple stmt in the if (i think need to use block?) 
  --3. very specific syntax needed to match
    ifStmt = 
      keyword "if" >>
      whitespace >>
      expr >>= \e ->
      whitespace >>

      blockStmt >>= \t -> --cant just use many1 to get all of them because If in W.hs needs a single Wstmt not a list 

      keyword "else">>

      blockStmt >>= \f -> -- same error for \t

      return (If e t f)

    whileStmt =  
      keyword "while" >>
      whitespace >>
      expr >>= \e ->
      whitespace >>

      blockStmt >>= \t ->

      return (While e t)


  --maybe we define the { } for both whilte and for loops for hte blbock and not in them
    blockStmt = 
      symbol "{" +++ symbol "{\n" >>
      many1 stmt >>= \e ->
      symbol "}" >>
      return (Block e)      

    -- the only kind of expression supported for now is stringLiterals
    -- implement the full expression language of W
    expr = stringLiteral +++ term >>= termSeq -- i think we need the +++ with more expresions of W
    --expr = term >>= termSeq
    -- stringLiterals can contain \n characters
    stringLiteral = (
                     char ('"') >>
                    many stringChar >>= \s ->
                    char ('"') >>
                    whitespace >>
                    return (Val (VString s))
                    ) +++
                    ( identifier >>= \i ->
                      return (Var i)) 

    stringChar = (char '\\' >> char 'n' >> return '\n') 
                 +++ sat (/= '"')

  
--these are where the grammers start shown in lecture 
    termSeq left = ( (symbol "+" +++ symbol "-"+++ 
                    symbol ">="+++ symbol "<="+++ 
                    symbol "<" +++ symbol ">" +++ 
                    symbol "==" +++ symbol "=" +++ symbol "!=") >>= \s ->
                 term >>= \right ->
                 termSeq ((toOp s) left right)
               ) +++ return left

    term = factor >>= factorSeq 

    factorSeq left = ( (symbol "*" +++ symbol "/") >>= \s ->
                      factor >>= \right ->
                      factorSeq ((toOp s) left right)
                    ) +++ return left

    factor = ( nat >>= \i -> --either get many digits 
              return (Val (VInt i))
            ) +++ 
            ( identifier >>= \i -> --get a variable name
            return (Var i)) 
            +++ parens expr

    toOp "+" = Plus
    toOp "-" = Minus
    toOp "*" = Multiplies
    toOp "/" = Divides
    toOp "<" = Less
    toOp ">" = Greater
    toOp "<=" = LessOrEqual
    toOp ">=" = GreaterOrEqual
    toOp "==" = Equals
    toOp "!=" = NotEqual
    ----------------------
    -- Parser utilities --
    ----------------------

    keywords = words "var if else while print"
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
