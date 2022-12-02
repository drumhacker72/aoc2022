import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepEndBy1)
import Text.Megaparsec.Char (char, eol)

import qualified Data.ByteString as B

type Parser = Parsec Void Text

data C1 = A | B | C
    deriving (Enum, Show)
data C2 = X | Y | Z
    deriving (Enum, Show)

type Round = (C1, C2)

column1 :: Parser C1
column1 = (char 'A' *> pure A) <|> (char 'B' *> pure B) <|> (char 'C' *> pure C)

column2 :: Parser C2
column2 = (char 'X' *> pure X) <|> (char 'Y' *> pure Y) <|> (char 'Z' *> pure Z)

oneRound :: Parser Round
oneRound = do
    c1 <- column1
    void $ char ' '
    c2 <- column2
    pure (c1, c2)

strategy :: Parser [Round]
strategy = sepEndBy1 oneRound eol

data Shape = Rock | Paper | Scissors
    deriving (Enum, Eq, Show)

strength, weakness :: Shape -> Shape
strength Rock = Scissors
strength s = pred s
weakness Scissors = Rock
weakness s = succ s

data Outcome = Loss | Draw | Win
    deriving (Enum, Show)

against :: Shape -> Shape -> Outcome
a `against` b
    | a == b = Draw
    | strength a == b = Win
    | otherwise = Loss

shapeScore :: Shape -> Int
shapeScore = (+ 1) . fromEnum

outcomeScore :: Outcome -> Int
outcomeScore = (* 3) . fromEnum

roundScore1 :: Round -> Int
roundScore1 (c1, c2) = shapeScore myShape + outcomeScore outcome
  where
    theirShape = toEnum $ fromEnum c1
    myShape = toEnum $ fromEnum c2
    outcome = myShape `against` theirShape

pickShape :: Outcome -> Shape -> Shape
pickShape Draw = id
pickShape Loss = strength
pickShape Win = weakness

roundScore2 :: Round -> Int
roundScore2 (c1, c2) = shapeScore myShape + outcomeScore outcome
  where
    theirShape = toEnum $ fromEnum c1
    outcome = toEnum $ fromEnum c2
    myShape = pickShape outcome theirShape

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day2.txt"
    let rounds = maybe (error "bad input") id $ parseMaybe strategy contents
    print $ sum $ map roundScore1 rounds
    print $ sum $ map roundScore2 rounds
