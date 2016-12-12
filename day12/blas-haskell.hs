{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M
import Data.Attoparsec.Text
import Control.Applicative hiding(many)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

data Ins = Cpy Integer Char
  | Mv Char Char
  | Jmp Integer Integer
  | Inc Char
  | Dec Char
  | Jnz Char Integer deriving (Show, Eq)

startStatus = M.fromList [('a',0),('b',0),('c',0),('d',0),('i',0)]
startStatus2 = M.fromList [('a',0),('b',0),('c',1),('d',0),('i',0)]

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

execute state (Jmp v1 v2) = M.insert 'i' newIp state
  where newIp = if v1 /= 0 then ip + v1 else ip + 1
        ip = (getVal 'i' state)

execute state (Jnz c val) = M.insert 'i' newIp state
  where newIp = if testZ then ip + val else ip + 1
        testZ = (getVal c state) /= 0
	ip = (getVal 'i' state)

execute state (Mv a b) = incIp $ M.insert b valA state
  where valA = getVal a state

incIp m = M.insert 'i' ip m
  where ip = (getVal 'i' m) + 1


getVal k m = case M.lookup k m of
		Just v -> v
		Nothing -> toInteger $ read (k:"")

parseCpy = do
  val <- "cpy " *> signed decimal
  space
  reg <- anyChar 
  return $ Cpy val reg

parseMv = do
  val <- "cpy " *> anyChar
  space
  reg <- anyChar 
  return $ Mv val reg
 
parseInc = do
  reg <- "inc " *> anyChar
  return $ Inc reg

parseDec = do
  reg <- "dec " *> anyChar 
  return $ Dec reg

parseJnz = do
  reg <- "jnz " *> anyChar
  space
  val <- signed decimal
  return $ Jnz reg val

parseIns = do
  parseCpy
  <|> parseInc
  <|> parseDec
  <|> parseJnz
  <|> parseMv


--parseText t= mapM (parseOnly parseIns) t 
parseText t= case mapM (parseOnly parseIns) t of
		Right x -> interpret startStatus2 x
		Left s -> error $ s

main = do
  text <- TIO.getContents
  print $ parseText $ T.lines text

