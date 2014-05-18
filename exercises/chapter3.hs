import Prelude hiding (sum)

-- exercise 9
data Direction = Straight | RightTurn | LeftTurn
    deriving (Show)

-- exercise 10
data Point = Point Double Double
    deriving (Show)

-- vector difference
diff :: Point -> Point -> Point
diff (Point x y) (Point x' y') = Point (x - x') (y - y')

-- vector cross product z coordinate
-- =0 = straight
-- >0 = left turn
-- <0 = right turn
crossZ :: Point -> Point -> Double
crossZ (Point x y) (Point x' y') = (x * y') - (y * x')

-- vector direction
dir :: Point -> Point -> Direction
dir v w = case compare z 0 of
            EQ -> Straight
            LT -> RightTurn
            GT -> LeftTurn
    where z = crossZ v w

direction :: Point -> Point -> Point -> Direction
direction p q r = dir v w
    where v = diff q p
          w = diff r q

-- exercise 11
directions :: [Point] -> [Direction]
directions [] = []
directions [_] = []
directions [_, _] = []
directions (p:q:r:ss) = direction p q r : directions (q:r:ss)
