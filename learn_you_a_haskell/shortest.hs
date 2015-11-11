data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
	let (bestA, bestB) = foldl roadStep ([],[]) roadSystem
	in if sum (map snd bestA) <= sum (map snd bestB)
		then reverse bestA
		else reverse bestB

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (toA, toB) s = (newToA, newToB)
	where 	
		pA = sum $ map snd toA
		pB = sum $ map snd toB
		aA = getA s + pA
		bA = getB s + getC s + pB
		bB = getB s + pB
		aB = getA s + getC s + pA
		newToA = if aA < bA then (A, getA s):toA else (C, getC s):(B, getB s):toB
		newToB = if bB < aB then (B, getB s):toB else (C, getC s):(A, getA s):toA
