import Data.List (sortBy, transpose, concatMap)
import Data.List.Split (chunksOf)
isTriangle (x:xs) = if x < sum xs then 1 else 0

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

solve1 :: [String] -> Int
solve1 = isTriangle . sortDesc . map read

solve2 = map solve1 . concatMap transpose . chunksOf 3

main = do
  text <- getContents
  print . sum . solve2 . map words . lines $ text
