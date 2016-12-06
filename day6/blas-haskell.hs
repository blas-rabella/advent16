import Data.Map (fromListWith, toList)
import Data.List (transpose, sortBy)

countFreq s = sortBy (\(_,b1) (_,b2) -> compare b2 b1) . toList . fromListWith (+) $ [(c,1)|c<-s]

solve1 = map (fst . head . countFreq) . transpose . lines
solve2 = map (fst . last . countFreq) . transpose . lines

main = do
  text <- getContents
  print $ solve2 text
