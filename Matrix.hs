module Matrix
(   
    Matrix
,   create
,   fold
,   rowfold
,   cols
,   rows
,   set
,   get
,   subMat
,   filterMult
) where

import Control.Monad
import Control.Applicative

data Matrix a = Mat [[a]]

instance (Show a) => Show (Matrix a) where
    show (Mat d) = concatMap r d where
        r x = concatMap ((++ "\t") . show) x ++ "\n"

instance Functor Matrix where
    fmap f (Mat d) = create $ map (map f) d

fold :: (a -> b -> a) -> a -> Matrix b -> a
fold f acc (Mat d) = foldl f acc (foldr (++) [] d)

rowfold :: (a -> b -> b) -> b -> Matrix a -> [b]
rowfold f acc (Mat d) = map (foldr f acc) d

create :: [[a]] -> Matrix a
create (d:ds) 
    | balanced (length d) ds    = Mat (d:ds)
    | otherwise                 = error "Matrix rows are imbalanced!"
    where balanced n = all $ (n ==) . length

set :: Matrix a -> (Int, Int) -> a -> Matrix a
set (Mat d) (n,m) r = Mat (replace d n (replace (d !! n) m r))

get :: Matrix a -> (Int, Int) -> a
get (Mat d) (n,m) = (d !! n) !! m

replace :: [a] -> Int -> a -> [a]
replace (d:ds) 0 new = (new:ds)
replace (d:ds) n new = d : (replace ds (n-1) new)

cols :: Matrix a -> Int
cols (Mat (d:ds)) = length d

rows :: Matrix a -> Int
rows (Mat m) = length m

subMat :: Matrix a -> (Int, Int) -> (Int, Int) -> Matrix a
subMat (Mat m) (x,y) (x',y') = create $ map (subList y y') (subList x x' m) where
    subList m n d = (take (n-m + 1) . drop m) d

mirrorHorizontal:: Matrix a -> Matrix a
mirrorHorizontal (Mat d) = create (map reverse d) 

filterMult :: (Num a) => Matrix a -> Matrix a -> (Int, Int) -> Matrix a
filterMult ker im (x,y) = elemwiseMult newKer newIm where
	elemwiseMult (Mat a) (Mat b) = create (zipWith (zipWith (*)) a b)
	newIm = subMat im (x-r, y-r) (x+r, y+r)
	newKer = subMat ker (r-x, r-y) (r-x + 2*r + 1, r-y +2*r +1)
	r = ((rows ker) - 1) `div` 2

main = print $ filterMult (create (replicate 3 [0.1,0.1,0.1])) (create (replicate 4 [1..4])) (3,3)


--rowfold (\p acc -> (if p then '*' else '.'):acc) ""  (create [[True,True],[False,True]])