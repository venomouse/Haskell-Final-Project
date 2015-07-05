import Matrix
import Data.List

type Image = Matrix Int

--instance Show Image where
--    show img = rowfold (\p acc -> (if p then '*' else '.'):acc) "" img

display :: Image -> String
display img = b ++ "\n" ++ intercalate "\n" r ++ "\n" ++ b ++ "\n" where 
    r = rowfold (\p acc -> (if (p==255) then "██" else "  ") ++ acc) "" img
    b = replicate ((cols img)*2) '-'

blank :: Int -> Int -> Image
blank n m = create (replicate n (replicate m 0))

main = putStr $ display (set (blank 100 100) 4 4 255)