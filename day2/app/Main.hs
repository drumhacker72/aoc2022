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

data Outcome = Loss | Draw | Win
    deriving (Enum, Show)

against :: Shape -> Shape -> Outcome
Rock `against` Scissors = Win
Scissors `against` Paper = Win
Paper `against` Rock = Win
a `against` b
    | a == b = Draw
    | otherwise = Loss

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore :: Outcome -> Int
outcomeScore Loss = 0
outcomeScore Draw = 3
outcomeScore Win = 6

roundScore1 :: Round -> Int
roundScore1 (c1, c2) = shapeScore myShape + outcomeScore outcome
  where
    theirShape = case c1 of
        A -> Rock
        B -> Paper
        C -> Scissors
    myShape = case c2 of
        X -> Rock
        Y -> Paper
        Z -> Scissors
    outcome = myShape `against` theirShape

pickShape :: Shape -> Outcome -> Shape
pickShape s Draw = s
pickShape Rock Loss = Scissors
pickShape Rock Win = Paper
pickShape Paper Loss = Rock
pickShape Paper Win = Scissors
pickShape Scissors Loss = Paper
pickShape Scissors Win = Rock

roundScore2 :: Round -> Int
roundScore2 (c1, c2) = shapeScore myShape + outcomeScore outcome
  where
    theirShape = case c1 of
        A -> Rock
        B -> Paper
        C -> Scissors
    outcome = case c2 of
        X -> Loss
        Y -> Draw
        Z -> Win
    myShape = pickShape theirShape outcome

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day2.txt"
    let rounds = maybe (error "bad input") id $ parseMaybe strategy contents
    print $ sum $ map roundScore1 rounds
    print $ sum $ map roundScore2 rounds
