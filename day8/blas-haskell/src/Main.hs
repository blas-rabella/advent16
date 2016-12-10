module Main where
import Parser
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List.Split (splitPlaces)
import Data.List (transpose)

data Status = On | Off  deriving (Eq, Read)
instance Show Status where
  show Off = " "
  show On = "█"
nLights n = take n $ repeat On

lightN n l = nLights n ++ others
  where (_,others) = splitAt n l

lightSquare n m l = turnedOn ++ b
  where (a,b) = splitAt m l
        turnedOn = map (lightN n) a

rotate n l = b ++ a 
  where (a,b) = splitAt w l
        w = (length l) - n

rotateMat n m l = (l'!!0) ++ rotated ++ end
  where l' = splitPlaces [n,1,rest] l
  	rest = length l
	rotated = map (rotate m) (l'!!1)
	end = if length l' == 3 then l'!!2 else []

rotateY n m = transpose . rotateMat n m . transpose

row n = take n . repeat

matrix n m = take n . repeat . row m

display = matrix 6 50 Off

execute (Rect a b) = lightSquare a b
execute (Rot X n m) = rotateY n m
execute (Rot Y n m) = rotateMat n m

interpret (Left _) = display
interpret (Right listActions) = foldl (\acc f -> execute f acc) (display) listActions

solve1 = length . filter (== On). concat . interpret . parseLines . T.lines
solve2 = interpret . parseLines . T.lines
main :: IO ()
main = do
  text <- TIO.getContents
  mapM_ print $ solve2 $ text
