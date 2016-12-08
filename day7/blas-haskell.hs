import Text.Regex.PCRE

isPalindrome :: String -> Bool
isPalindrome x = x == (reverse x) 

twoDifferent (x:y:xs) = x /= y
twoDifferent _ = False

isValidTLS s = twoDifferent s && isPalindrome s && 4 <= length s

checkTLS [] = False
checkTLS xs = if isValidTLS ff then True else checkTLS $ tail xs
	where ff = take 4 xs

check xs = oneTLS /= [] && noneTLS == []
   where oneTLS = filter (== True) . map checkTLS $ filter (hasBraket) xs
         noneTLS =  filter (== False) . map (not . checkTLS) $ filter (not . hasBraket) xs
         hasBraket s = elem '[' s || elem ']' s
solveLine :: String -> Bool
solveLine line = check . concat $ (line =~ "\\]?\\w+\\[?"  :: [[String]])

main = do
  text <- getContents
  print . length . filter (id) . map solveLine . lines $ text

