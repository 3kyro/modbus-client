module Repl.HelpFun
    ( getModByName
    , findModByName
    , getModByPair
    , insertModValue
    , getPairs
    , invalidCmd
    ) where

import Control.Monad.State.Strict (liftIO)
import Data.List (find)

import PrettyPrint (ppStrWarning)
import Repl.Parser (pReplDesc, pReplFloat, pReplWord)
import Types

-- Parse a ModData name and lookup in a list of ModData if it exists
getModByName :: [ModData] -> String -> Either AppError ModData
getModByName mdata x = do
    parsed <- pReplDesc x
    findModByName mdata parsed

-- Find a ModData by name
findModByName :: [ModData] -> String -> Either AppError ModData
findModByName mdata x = do
    let maybeHit = find (\d -> modName d == x) mdata
    case maybeHit of
        Nothing -> Left $ AppCommandError $ "Modbus Register " ++ x ++ " not found on template file."
        Just hit -> Right hit

-- Generate a list of Moddata from pairs of Description and values
getModByPair :: [(String, String)] -> [ModData] -> Either AppError [ModData]
getModByPair [] _ = Right []
getModByPair ((desc,val):xs) mds = do
    validMd <- getModByName mds desc
    inserted <- insertModValue validMd val
    (:) <$> return inserted <*> getModByPair xs mds

-- Parses and inserts a ModValue to a ModData
insertModValue :: ModData -> String -> Either AppError ModData
insertModValue md val =
    case modValue md of
        ModWord _ -> do
            parsed <- pReplWord val
            return $ md {modValue = ModWord (Just parsed)}
        ModFloat _ -> do
            parsed <- pReplFloat val
            return $ md {modValue = ModFloat (Just parsed)}

-- Returns a pair of successive values
-- Returns Nothing if the list size is odd
getPairs :: [a] -> Maybe [(a,a)]
getPairs [] = Just []
getPairs [_] = Nothing
getPairs (x:y:zs) = (:) <$> Just (x,y) <*> getPairs zs

invalidCmd :: Repl ()
invalidCmd = liftIO $ ppStrWarning "Invalid Command argument"
