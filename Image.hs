import Matrix
import Data.List

type Image = Matrix Bool

--instance Show Image where
--    show img = rowfold (\p acc -> (if p then '*' else '.'):acc) "" img

display :: Image -> String
display img = b ++ "\n" ++ intercalate "\n" r ++ "\n" ++ b ++ "\n" where 
    r = rowfold (\p acc -> (if p then '█' else ' '):acc) "" img
    b = replicate (cols img) '-'

blank :: Int -> Int -> Image
blank n m = create (replicate n (replicate m False))

main = putStr $ display (set (blank 10 10) 4 4 True)