module Repl.Parser 
    (
      pReplAddrNum
    , pReplFloat
    , pReplWord      
    ) 
    where

import Control.Monad.IO.Class ()
import Data.Word (Word16)
import Data.Either.Combinators (mapLeft)
import Text.Parsec (parse, eof, option, string)
import Text.Parsec.Token (float, decimal, makeTokenParser)
import Text.Parsec.Language (haskellDef)

import Repl.Error (ReplError (..))

-- Parse address and number of register strings 
pReplAddrNum :: String -> String-> Either ReplError (Word16, Word16)
pReplAddrNum a n = (,) <$> pReplWord a <*> pReplWord n

pReplFloat :: String -> Either ReplError Float
pReplFloat s = mapLeft ReplParseError parseResult
  where 
    parseResult = parse (pRawFloat <* eof) "" s
    pRawFloat = read <$> ((++) <$> sign <*> pf)
    sign = option "" (string "-")
    pf = show <$> float (makeTokenParser haskellDef)   
        
pReplWord :: String -> Either ReplError Word16
pReplWord s = mapLeft ReplParseError parseResult  
  where
    parseResult = parse (pRawWord <* eof) "" s
    pRawWord = fromInteger <$> decimal (makeTokenParser haskellDef)


