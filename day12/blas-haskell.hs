{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M
import Data.Attoparsec.Text
import Control.Applicative hiding(many)

data Ins = Cpy Integer Char
  | Inc Char
  | Dec Char
  | Jnz Char Integer deriving (Show, Eq)

startStatus = M.fromList [('a',0),('b',0),('c',0),('d',0),('i',0)]

interpret m l 
  | (getVal 'i' m) == (toInteger $ length l) = getVal 'a' m
  | otherwise = interpret newStatus l
  	where newStatus = execute m (l!!(fromInteger ip::Int))
	      ip = (getVal 'i' m)

execute state (Inc c) = incIp $ M.insert c newVal state
  where newVal = (getVal c state) + 1

execute state (Dec c) = incIp $ M.insert c newVal state
  where newVal = (getVal c state) - 1

execute state (Cpy val c) = incIp $ M.insert c val state

execute state (Jnz c val) = M.insert 'i' newIp state
  where newIp = if testZ then ip + val else ip + 1
        testZ = (getVal c state) /= 0
	ip = (getVal 'i' state)

incIp m = M.insert 'i' ip m
  where ip = (getVal 'i' m) + 1

getVal k m = case M.lookup k m of
		Just v -> v
		Nothing -> error "Key not Found"

parseCpy = do
  val <- "cpy " *> decimal
  space
  reg <- anyChar <* endOfLine
  return $ Cpy val reg
 
parseInc = do
  reg <- "inc " *> anyChar <* endOfLine
  return $ Inc reg

parseDec = do
  reg <- "inc " *> anyChar <* endOfLine
  return $ Dec reg

parseJnz = do
  reg <- "jnz " *> anyChar
  space
  val <- decimal <* endOfLine
  return $ Jnz reg val

parseIns = do
  parseCpy
  <|> parseInc
  <|> parseDec
  <|> parseJnz

parseText "" = []
parseText t = case parse parseIns of
	Done t' x -> x : parseText t'
	Fail _ _ y -> error $ y
