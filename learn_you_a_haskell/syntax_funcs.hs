lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, out of luck!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "One elem: " ++ show x
tell (x:y:[]) = "Two elems: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "Many elems..."

length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + (length' xs)

bmiTell :: (RealFloat x) => x -> x -> String
bmiTell weight height
	| bmi <= 18.5 = "Underweight"
	| bmi <= 25.0 = "Normal"
	| bmi <= 30.0 = "Fat"
	| otherwise = "Obese"
	where bmi = weight / height^2
