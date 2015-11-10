module types where

data Person = Person { firstname :: String
			, lastName :: String
			, age :: Int
			, height :: Float
			, phoneNumber :: String
			, flavor :: String
			} deriving (Shows)

data Car = Car {company :: String, model :: String, year :: Int}  deriving (Show)

data Vector a = Vector a a a deriving (Show)
vAdd :: (Num t) => Vector t -> Vector t -> Vector t
vAdd (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

vMul :: (Num t) => Vector t -> Vector t -> Vector t
vMul (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1*x2) (y1*y2) (z1*z2)

scalarMul :: (Num t) => Vector t -> t -> Vector t
scalarMul (Vector x1 y1 z1) k = Vector (x1*k) (y1*k) (z1*k)

dotProd :: (Num t) => Vector t -> Vector t -> t
dotProd v1 v2 = sumVec (vMul v1 v2)
	where sumVec (Vector x1 y1 z1) = x1 + y1 + z1 

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True
instance YesNo [a] where
	yesno [] = False
	yesno _ = True
instance YesNo Bool where
	yesno = id
instance YesNo (Maybe a) where 
	yesno (Just _) = True
	yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesRes noRes = if yesno yesnoVal then yesRes else noRes

