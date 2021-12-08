import Data.Char

import Parsing

myParser = do x <- sat isUpper
              y <- sat isLower
              return (if x==toUpper y then x:[y] else [])



openBr = char '['
closeBr = char ']'
openPn = char '('
closePn = char ')'
star = char '*'
myParser2 :: Parser [Char]
myParser2 = (many1 star >>= \vs -> myParser >>= \x -> return (x + (length vs))) +++
  (openBr >> myParser >>= \c -> closeBr >>
    myParser >>= \d -> return (c + d)) +++
  (openPn >> myParser >>= \c -> closePn >>
    myParser >>= \d -> return (c + d)) +++
  (return 0)