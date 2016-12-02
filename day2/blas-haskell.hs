import qualified Data.Matrix as M

data Move = U | D | L | R deriving (Show, Read, Eq)
type Point = (Int,Int)

sub 1 = 1
sub x = pred x

add 5 = 5
add x = succ x

doMove (x,y) m
  | m == U = (sub x, y)
  | m == D = (add x, y)
  | m == L = (x, sub y)
  | otherwise = (x, add y)

doMove2 p m = if (pad2 M.! newPoint) /= "X" then newPoint else p
  where newPoint = doMove p m
  
makeMoves :: (Point -> Move -> Point) -> Point -> [Move] -> Point
makeMoves = foldl

solveLine f point line = makeMoves f point instructions
	where instructions = map (read . (:[])) line

solve f point lines = tail $ scanl (solveLine f) point lines
solve1 point lines = solve doMove point lines
solve2 point lines = solve doMove2 point lines

pad = M.fromList 3 3 [1..] 

pad2 = M.fromList 5 5 ["X", "X", "1", "X", "X"
                      ,"X", "2", "3", "4", "X"
                      ,"5", "6", "7", "8", "9"
                      ,"X", "A", "B", "C", "X"
                      ,"X", "X", "D", "X", "X"]


showSolution = map (\x -> pad2 M.! x)
 
main = do
  text <- getContents
  print . showSolution . solve2 (3,3). lines $ text
