import Data.List (foldl')
import Data.Sequence (Seq(..))

import qualified Data.Sequence as S

type File = Seq (Int, Int)

focus :: ((Int, Int) -> Bool) -> File -> File
focus _ Empty = error "unexpected: empty file"
focus p s@(x :<| xs)
    | p x = s
    | otherwise = focus p (xs :|> x)

focusIdx, focusVal :: Int -> File -> File
focusIdx i = focus (\(p, _) -> p == i)
focusVal v = focus (\(_, x) -> v == x)

move :: File -> File
move Empty = error "unexpected: empty file"
move (x@(_, n) :<| xs) = let (s1, s2) = S.splitAt (n `mod` length xs) xs in x :<| s2 <> s1

indexWrapped :: File -> Int -> Int
indexWrapped xs i = snd $ xs `S.index` (i `mod` length xs)

mix :: File -> File
mix file = foldl' (\acc n -> move $ focusIdx n acc) file [1..length file]

main :: IO ()
main = do
    (nums :: [Int]) <- map read . lines <$> readFile "day20.txt"
    let file = S.fromList $ zip [1..] nums
        file' = focusVal 0 $ mix file
    print $ sum $ map (indexWrapped file') [1000, 2000, 3000]

    let file2 = S.fromList $ zip [1..] $ map (* 811589153) nums
        file2' = focusVal 0 $ iterate mix file2 !! 10
    print $ sum $ map (indexWrapped file2') [1000, 2000, 3000]
