import Data.Char (ord)
import Data.List (intersect, nub)

compartmentalize :: String -> (String, String)
compartmentalize s = splitAt (length s `div` 2) s

findDuplicate :: (String, String) -> Char
findDuplicate (a, b) = case nub (intersect a b) of
    [x] -> x
    _ -> error $ "expected exactly one duplicate"

priority :: Char -> Int
priority c
    | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
    | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
    | otherwise = error "unexpected item"

groupBy3 :: [String] -> [(String, String, String)]
groupBy3 [] = []
groupBy3 (a:b:c:rest) = (a, b, c) : groupBy3 rest
groupBy3 _ = error "uneven groups"

findBadge :: (String, String, String) -> Char
findBadge (a, b, c) = case nub (intersect (intersect a b) c) of
    [x] -> x
    _ -> error "expected exactly one shared item"

main :: IO ()
main = do
    rucksacks <- lines <$> readFile "day3.txt"
    print $ sum $ map (priority . findDuplicate . compartmentalize) rucksacks
    print $ sum $ map (priority . findBadge) $ groupBy3 rucksacks
