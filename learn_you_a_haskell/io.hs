import Data.Char

main = do

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\line -> if isPalindrome line then "Pali!" else "Not Pali!") . lines 
	where isPalindrome = 
