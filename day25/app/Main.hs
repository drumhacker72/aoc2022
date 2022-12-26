readSnafu :: Int -> String -> Int
readSnafu n [] = n
readSnafu n (c:s) = readSnafu (5 * n + d) s
  where
    d = case c of
        '=' -> -2
        '-' -> -1
        '0' -> 0
        '1' -> 1
        '2' -> 2
        _ -> error "unexpected character"

data D5 = Zero | One | Two | Three | Four
    deriving (Enum, Show)

balance1 :: D5 -> Bool -> (Int, Bool)
balance1 Zero carry = (fromEnum carry, False)
balance1 One carry = (1 + fromEnum carry, False)
balance1 Two False = (2, False)
balance1 Two True = (-2, True)
balance1 Three carry = (-2 + fromEnum carry, True)
balance1 Four carry = (-1 + fromEnum carry, True)

balance :: [D5] -> [Int]
balance ds =
    let (bal, c) = foldl (\(bs, carry) d -> let (b, carry') = balance1 d carry in (b:bs, carry')) ([], False) ds
     in if c then 1:bal else bal

toBase5 :: Int -> [D5] -> [D5]
toBase5 0 ds = reverse ds
toBase5 n ds = let (n', d) = n `divMod` 5 in toBase5 n' (toEnum d : ds)

showSnafu :: [Int] -> String -> String
showSnafu [] s = s
showSnafu (n:ns) s = showSnafu ns (showDigit n:s)
  where
    showDigit (-2) = '='
    showDigit (-1) = '-'
    showDigit 0 = '0'
    showDigit 1 = '1'
    showDigit 2 = '2'
    showDigit _ = error "unexpected digit"

main :: IO ()
main = do
    ns <- map (readSnafu 0) . lines <$> readFile "day25.txt"
    let r = sum ns
        b5 = toBase5 r []
        bal = balance b5
    putStrLn $ showSnafu (reverse bal) ""
