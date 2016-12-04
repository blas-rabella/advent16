import qualified Data.Map as M
import Data.List (sortBy, isPrefixOf, isInfixOf)
import Data.List.Split (oneOf, split, dropBlanks)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Char (ord, chr)
countFreq s = M.toList $ M.fromListWith (+) [(c,1)| c<-s, c /= '-']

sorting (a1,b1) (a2,b2) = if compar == EQ then compare a2 a1 else compar
	where compar = compare b1 b2

splitter = split (dropBlanks $ oneOf ['0'..'9']) 

isReal letters key = isPrefixOf checksum processed
	where processed = map fst . sortBy (flip sorting) . countFreq $ letters
	      checksum = [x|x<-key, x /= '[', x /= ']']

solveLine string = if isReal letters key then Just number else Nothing
	where letters:number:key:rest = splitter string

solve1 = sum . map read . catMaybes . map solveLine . lines

char2int c = ord c - ord 'a'
int2char n = chr $ (ord 'a') + n

decode _ '-' = ' '
decode n c = int2char . (flip mod 26) $ (char2int c) + n 

solveLine2 text = if isInfixOf "north" decrypted then Just (n,decrypted) else Nothing 
	where s:number:rest = splitter text
	      n = read number
	      decrypted = map (decode n) s

solve2 = mapMaybe solveLine2 . lines

main = do
  text <- getContents
  print $ solve2 text
