module Matrix
(   
    Matrix
,   create
,   fold
,   rowfold
,   cols
,   set
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

set :: Matrix a -> Int -> Int -> a -> Matrix a
set (Mat d) n m r = Mat (replace d n (replace (d !! n) m r))

replace :: [a] -> Int -> a -> [a]
replace (d:ds) 0 new = (new:ds)
replace (d:ds) n new = d : (replace ds (n-1) new)

cols :: Matrix a -> Int
cols (Mat (d:ds)) = length d

main = print $ set (create [[True,True],[False,True]]) 0 1 False
--rowfold (\p acc -> (if p then '*' else '.'):acc) ""  (create [[True,True],[False,True]])