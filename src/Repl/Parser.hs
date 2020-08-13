module Repl.Parser 
    (
      pReplAddrNum
    , pReplFloat
    , pReplWord    
    , pReplDesc  
    , pReplId
    , pReplInt
    ) 
    where


import Control.Monad.IO.Class ()
import Data.Word (Word16)
import Data.Either.Combinators (mapLeft)
import Text.Parsec ((<|>),parse)
import Text.Parsec.Text (Parser)

import qualified Data.Text as T

import CsvParser (pInt, pFloat, only, pName, pWord16)
import Repl.Error (ReplError (..))
import Repl.Types (ReplIdent (..))

    
-- Parse address and number of register strings 
pReplAddrNum :: String -> String-> Either ReplError (Word16, Word16)
pReplAddrNum a n = (,) <$> pReplWord a <*> pReplWord n

pReplFloat :: String -> Either ReplError Float
pReplFloat = replConvParser pFloat
        
pReplWord :: String -> Either ReplError Word16
pReplWord = replConvParser pWord16

pReplInt :: String -> Either ReplError Int
pReplInt = replConvParser pInt

pReplDesc :: String -> Either ReplError String
pReplDesc = replConvParser (only pName)

pReplId :: String -> Either ReplError ReplIdent
pReplId = replConvParser parser 
  where
      parser = desc <|> addr
      desc = ReplDesc <$> pName
      addr = ReplAddr <$> pWord16

replConvParser :: Parser a -> String -> Either ReplError a
replConvParser p s = mapLeft ReplParseError $ parse (only p) "" $ T.pack s