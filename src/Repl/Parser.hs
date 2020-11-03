module Repl.Parser
    (
      pReplAddrNum
    , pReplFloat
    , pReplWord
    , pReplDesc
    , pReplArg
    , pReplInt
    ,pReplWordBit)
    where

import           Control.Monad.IO.Class  ()
import           Data.Either.Combinators (mapLeft)
import           Data.Word               (Word16)
import           Text.Parsec             (parse, (<|>))
import           Text.Parsec.Text        (Parser)

import qualified Data.Text               as T

import           CsvParser               (only, pFloat, pInt, pName, pWord, pBit)

import           Types                   (WordBit, AppError(..), ReplArg (..))


-- Parse address and number of register strings
pReplAddrNum :: String -> String-> Either AppError (Word16, Word16)
pReplAddrNum a n = (,) <$> pReplWord a <*> pReplWord n

pReplFloat :: String -> Either AppError Float
pReplFloat = replConvParser pFloat

pReplWord :: Read a => String -> Either AppError a
pReplWord = replConvParser pWord

pReplInt :: String -> Either AppError Int
pReplInt = replConvParser pInt

pReplWordBit :: String -> Either AppError WordBit
pReplWordBit = replConvParser pBit

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
