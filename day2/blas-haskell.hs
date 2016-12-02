import qualified Data.Matrix as M

data Move = U | D | L | R deriving (Show, Read, Eq)
type Point = (Int,Int)

sub 0 = 0
sub x = pred x

add 2 = 2
add x = succ x

doMove (x,y) m
  | m == U = (x,sub y)
  | m == D = (x,add y)
  | m == L = (sub x, y)
  | otherwise = (add x, y)

makeMoves :: Point -> [Move] -> Point
makeMoves = foldl doMove

solveLine point line = makeMoves point instructions
	where instructions = map (read . (:[])) line

solve1 point lines = tail $ scanl solveLine point lines

pad = M.fromList 3 3 [1..] 

showSolution = map (\x -> pad ! x)
