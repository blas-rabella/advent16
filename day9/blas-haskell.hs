{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text as T
import qualified Control.Applicative as A
import qualified Data.Text.IO as TIO

data Chunk = Repeat Integer T.Text | Keep T.Text deriving (Eq, Show, Read)
type ParsedIn = [Chunk]
parseRepeat = do
  n <- "(" A.*> AT.decimal A.<* "x"
  m <- AT.decimal A.<* ")"
  blob <- AT.take n
  return $ Repeat m blob

parseBlob = do
  blob <- AT.takeTill (AT.inClass "(")
  return $ Keep blob

parseChunk = do
  parseRepeat A.<|> parseBlob

parseText text = 
  case AT.parse parseChunk text of 
	AT.Fail i _ y -> error $ T.unpack i
	AT.Done text' x -> x : parseText text'
	r@(AT.Partial x) -> unwrapPartial r

unwrapPartial r = 
  case AT.feed r "" of
  	AT.Fail _ _ y -> error $ y
	AT.Done _ x -> [x]
	AT.Partial _ -> []

execute :: Chunk -> Integer 
execute (Keep t) = toInteger . T.length $ T.filter (/= '\n') t
execute (Repeat n t) =  toInteger . sum $ map (* n) (solve2 t) 

solve2 :: T.Text -> [Integer]
solve2 l = map execute $ parseText l

main = do
  text <- TIO.getContents
  print $ sum $solve2 text
