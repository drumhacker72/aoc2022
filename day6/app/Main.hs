import Data.List (nub)

findMarker :: Int -> Int -> String -> Int
findMarker count pos buffer
    | length (nub (take count buffer)) == count = pos
    | otherwise = findMarker count (pos+1) (tail buffer)

main :: IO ()
main = do
    [buffer] <- lines <$> readFile "day6.txt"
    print $ findMarker 4 4 buffer
    print $ findMarker 14 14 buffer
