module Main where
import Parser
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List.Split (splitPlaces)
import Data.List (transpose)

data Status = On | Off deriving (Eq, Show, Read)

nLights n = take n $ repeat On

lightN n l = nLights n ++ others
  where (_,others) = splitAt n l

lightSquare n m l = turnedOn ++ b
  where (a,b) = splitAt m l
        turnedOn = map (lightN n) a

rotate n l = b ++ a 
  where (a,b) = splitAt w l
        w = (length l) - n

rotateMat n m l = (l'!!0) ++ rotated ++ (l'!!2)
  where l' = splitPlaces [n,1,rest] l
  	rest = length l
	rotated = map (rotate m) (l'!!1)

rotateY n m = transpose . rotateMat n m . transpose

row n = take n . repeat

matrix n m = take n . repeat . row m

display = matrix 6 50 Off

execute (Rect a b) = lightSquare a b
execute (Rot X n m) = rotateY n m
execute (Rot Y n m) = rotateMat n m

interpret (Left _) = [(display, Rect 0 0)]
interpret (Right listActions) = scanl (\(acc,op) f -> (execute f acc, f)) (display, Rect 0 0) listActions

main :: IO ()
main = do
  text <- TIO.getContents
  print $ matrix 3 7 Off
  print $ interpret . parseLines . T.lines $ text
