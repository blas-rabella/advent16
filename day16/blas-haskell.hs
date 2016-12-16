import Data.List.Split (chunksOf)
f a = a ++ [0] ++ b
  where b = map (inv) . reverse $ a

inv 0 = 1
inv 1 = 0
inv _ = 2

checksum l = if mod (length l) 2 == 0 then checksum divideF else l
  where divideF = map (\(x:y:_) -> if x == y then 1 else 0) . chunksOf 2 $ l

fillDisk k l = if length l < k then fillDisk k $ f l else l


solve1 l k = checksum $ fillDisk k l
