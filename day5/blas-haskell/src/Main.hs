import Data.Hash.MD5 
import Data.List (isPrefixOf, sortBy)
import qualified Data.Map as M
hash s = md5s $ Str s

genBlocks s = [h | n <- [0..], let h = hash (s++(show n)), isPrefixOf "00000" h]

solve1 s = map (!!5) . take 8 $ genBlocks s

getPair l = (l!!5,l!!6)

solve2 s =  M.elems . head . dropWhile (not . valid) $ scanl add M.empty ks
  where add m (k,v) = if M.member k m then m else M.insert k v m
        valid m =  (M.keys m) == ['0'..'7']
        ks = [getPair x | x <- genBlocks s, (x!!5) < '8', (x!!5)>='0']

main :: IO()
main = do
  text <- getLine
  print $ solve2 text
