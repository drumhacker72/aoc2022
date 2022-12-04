import Data.Functor (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy1)
import Text.Megaparsec.Char (char, eol)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

assignment :: Parser (Int, Int)
assignment = do
    start <- L.decimal
    void $ char '-'
    end <- L.decimal
    pure (start, end)

pair :: Parser ((Int, Int), (Int, Int))
pair = do
    first <- assignment
    void $ char ','
    second <- assignment
    pure (first, second)

input :: Parser [((Int, Int), (Int, Int))]
input = sepEndBy1 pair eol

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
(s1, e1) `fullyContains` (s2, e2) = s1 <= s2 && e2 <= e1

eitherFullyContains :: (Int, Int) -> (Int, Int) -> Bool
eitherFullyContains a b = (a `fullyContains` b) || (b `fullyContains` a)

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (s1, e1) (s2, e2) = case s1 `compare` s2 of
    LT -> e1 >= s2
    EQ -> True
    GT -> e2 >= s1

main :: IO ()
main = do
    contents <- readFile "day4.txt"
    let assignmentPairs = maybe (error "bad input") id $ parseMaybe input contents
    print $ length $ filter id $ map (uncurry eitherFullyContains) assignmentPairs
    print $ length $ filter id $ map (uncurry overlap) assignmentPairs
