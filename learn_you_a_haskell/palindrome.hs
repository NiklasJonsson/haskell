import Data.Char

main = interact respondPalindromes

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "Pali!" else "Not Pali!") . lines  
	where 	isPalindrome xs = xs == reverse xs
