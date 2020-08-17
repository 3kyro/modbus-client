module Repl.Parser 
    (
      pReplAddrNum
    , pReplFloat
    , pReplWord    
    , pReplDesc
    , pReplArg
    , pReplInt
    ) 
    where

import Control.Monad.IO.Class ()
import Data.Word (Word16)
import Data.Either.Combinators (mapLeft)
import Text.Parsec ((<|>),parse)
import Text.Parsec.Text (Parser)

import qualified Data.Text as T

import CsvParser (pInt, pFloat, only, pName, pWord)
import Repl.Error (AppError (..))

import Types (ReplArg (..))

    
-- Parse address and number of register strings 
pReplAddrNum :: String -> String-> Either AppError (Word16, Word16)
pReplAddrNum a n = (,) <$> pReplWord a <*> pReplWord n

pReplFloat :: String -> Either AppError Float
pReplFloat = replConvParser pFloat
        
pReplWord :: String -> Either AppError Word16
pReplWord = replConvParser pWord

pReplInt :: String -> Either AppError Int
pReplInt = replConvParser pInt

pReplDesc :: String -> Either AppError String
pReplDesc = replConvParser (only pName)

pReplArg :: String -> Either AppError ReplArg
pReplArg = replConvParser parser
  where
      parser = name <|> addr
      name = ReplName <$> pName
      addr = ReplAddr <$> pWord

replConvParser :: Parser a -> String -> Either AppError a
replConvParser p s = mapLeft AppParseError $ parse (only p) "" $ T.pack s