import Text.Regex.PCRE
import Data.List (isInfixOf,intercalate,reverse)

isPalindrome :: String -> Bool
isPalindrome x = x == (reverse x) 

twoDifferent (x:y:xs) = x /= y
twoDifferent _ = False

isValidABA s = twoDifferent s && isPalindrome s && 3 <= length s

complement (x:y:_) = y:x:y:[]
complement _ = []

checkHasABA [] _ = False
checkHasABA hyper net = if validBAB then True else checkHasABA (tail net) hyper
    where ft = take 3 net
          validBAB = isValidABA ft && isInfixOf (complement ft) hyper
first [] = False
first xs = head xs
check xs = first . dropWhile (not . id) $ map (checkHasABA hyper) nets
	where nets = filter (hasBraket) xs
              hyper =  intercalate "*" $ filter (not . hasBraket) xs
              hasBraket s = elem '[' s || elem ']' s

solveLine :: String -> Bool
solveLine line = check . concat $ (line =~ "\\]?\\w+\\[?"  :: [[String]])

main = do
  text <- getContents
  print . length . filter (id) . map solveLine . lines $ text

