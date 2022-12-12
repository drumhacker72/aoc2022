import Data.Foldable (toList)
import Data.Functor ((<&>), void)
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..), (|>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), optional, parseMaybe, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)

import qualified Data.Sequence as S
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Operation
    = Add !(Maybe Int) !(Maybe Int)
    | Mul !(Maybe Int) !(Maybe Int)
    deriving Show

data Monkey = Monkey
    { items :: Seq Int
    , operation :: Operation
    , test :: Int
    , true :: Int
    , false :: Int
    , inspections :: Int
    } deriving Show

pOperand :: Parser (Maybe Int)
pOperand = (string "old" *> pure Nothing) <|> (L.decimal <&> Just)

pOperation :: Parser Operation
pOperation = do
    a <- pOperand
    (string " + " *> pOperand <&> Add a) <|> (string " * " *> pOperand <&> Mul a)

pMonkey :: Parser (Int, Monkey)
pMonkey = do
    void $ string "Monkey "
    n <- L.decimal
    void $ char ':'
    void eol
    void $ string "  Starting items: "
    items <- S.fromList <$> sepBy1 L.decimal (string ", ")
    void eol
    void $ string "  Operation: new = "
    op <- pOperation
    void eol
    void $ string "  Test: divisible by "
    test <- L.decimal
    void eol
    void $ string "    If true: throw to monkey "
    t <- L.decimal
    void eol
    void $ string "    If false: throw to monkey "
    f <- L.decimal
    void $ optional eol
    pure $ (n, Monkey items op test t f 0)

doOperation :: Int -> Operation -> Int
doOperation worry (Add a b) = fromMaybe worry a + fromMaybe worry b
doOperation worry (Mul a b) = fromMaybe worry a * fromMaybe worry b

doItem :: (Int -> Int) -> Int -> Seq Monkey -> Int -> Seq Monkey
doItem relief i monkeys worry = S.adjust' (\m -> m { items = items m |> worry' }) dest monkeys
  where
    src = monkeys `S.index` i
    worry' = relief $ doOperation worry (operation src)
    dest = if (worry' `mod` test src == 0) then true src else false src

doTurn :: (Int -> Int) -> Seq Monkey -> Int -> Seq Monkey
doTurn relief monkeys i = S.update i m' $ foldl' (doItem relief i) monkeys (items m)
  where
    m = monkeys `S.index` i
    m' = m { items = S.empty, inspections = inspections m + length (items m) }

doRound :: (Int -> Int) -> Seq Monkey -> Seq Monkey
doRound relief monkeys = foldl' (doTurn relief) monkeys [0..length monkeys - 1]

business :: Foldable t => t Monkey -> Int
business = product . take 2 . reverse . sort . map inspections . toList

main :: IO ()
main = do
    contents <- readFile "day11.txt"
    let monkeys = S.fromList
            $ map snd
            $ maybe (error "bad input") id
            $ parseMaybe (sepEndBy1 pMonkey eol) contents

    print $ business $ iterate (doRound (`div` 3)) monkeys !! 20

    let relief w = w `mod` foldl' lcm 1 (fmap test monkeys)
    print $ business $ iterate (doRound relief) monkeys !! 10_000
