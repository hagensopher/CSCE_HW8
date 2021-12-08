import Data.Char

import Parsing

myParser = do x <- sat isUpper
              y <- sat isLower
              return (if x==toUpper y then x:[y] else failure)