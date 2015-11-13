import Control.Monad

type KnightPos = (Int,Int)

moveKnight :: KnightPos ->  [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2, r+1),(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    guard (elem c' [1..8] && elem r' [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 from to = elem to (in3 from)

canReachIn3' from to = elem to $ in3 from

