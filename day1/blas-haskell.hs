{-# LANGUAGE ConstrainedClassMethods #-}
import Data.List.Split (splitOn)
import Data.Set as Set (insert, member, empty)
import Data.Maybe (mapMaybe)

data Direction = North | West | South | East deriving (Show, Enum, Eq, Bounded)
data Turn = L | R deriving (Show, Eq, Read)
type Point = (Int,Int)
type Segment = (Point,Point)

class Circular a where
  next :: Enum a => a -> a
  previous :: Enum a => a -> a

instance Circular Direction where
  next a = if a == maxBound then minBound else succ a
  previous a = if a == minBound then maxBound else pred a

toTuple :: Direction -> Point
toTuple North = (0,1)
toTuple South = (0,-1)
toTuple East = (1,0)
toTuple West = (-1,0)

turn t d 
 | t == L = next d
 | otherwise = previous d

applyF f (a,b) = (f a, f b)

sumTuple (a,b) (c,d) = (a+c,b+d)


turnAndWalk n t d p = (sumTuple p nextPos, newDir)
	where newDir = turn t d
	      nextPos = applyF (* n) . toTuple $ turn t d

run :: [(Int,Turn)] -> [(Point,Direction)]
run = scanl (\(p,d) (n,t) -> turnAndWalk n t d p) ((0,0),North)

parse :: String -> (Int, Turn)
parse s = (n,t)
	where t = read $ turnString
	      n = read $ number
	      (turnString, number) = splitAt 1 s

buildProblem = map parse . splitOn ", "

dist (a,b) = (abs a)+(abs b)

solution1 = dist . fst . last

cross :: Segment -> Segment -> Maybe Point
cross a@(p,q) b@(r,t) = if isVertical a then cross' b a else cross' a b
cross' s1 s2 = if doCross s1 s2 then Just (getX . fst $ s2, getY . fst $ s1) else Nothing

doCross (p,q) (r,t) = if inX p r q && inY r p t then True else False 

inF :: (Point -> Int) -> Point -> Point -> Point -> Bool
inF f a b c = (f a < f b) && (f b < f c)
inX = inF getX
inY = inF getY

isVertical (p,q) = (getX p) == (getX q)

getX = fst
getY = snd

allCrosses _ [] = []
allCrosses previous (x:xs) = crosses ++ (allCrosses (previous++[x]) xs)
	where crosses = mapMaybe (cross x) previous

buildSegments l = zip points (tail points)
	where points = map fst l

solution2 = dist . head . allCrosses [] . buildSegments

main = do
  line <- getContents
  print . solution2 . run . buildProblem $ line
