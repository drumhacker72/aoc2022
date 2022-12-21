{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (bimap)
import Data.Char (isLower)
import Data.Functor ((<&>), void)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol, string)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Operation = Add | Sub | Mul | Div
    deriving (Enum, Show)
data Job = Literal !Int | Result !Operation !(Text, Text)
    deriving Show

pOperation :: Parser Operation
pOperation = (char '+' *> pure Add) <|> (char '-' *> pure Sub) <|> (char '*' *> pure Mul) <|> (char '/' *> pure Div)

pName :: Parser Text
pName = takeWhile1P (Just "lowercase letter") isLower

pJob :: Parser Job
pJob = (L.decimal <&> Literal) <|> do
    a <- pName
    void $ char ' '
    op <- pOperation
    void $ char ' '
    b <- pName
    pure $ Result op (a, b)

pMonkeyJob :: Parser (Text, Job)
pMonkeyJob = do
    name <- pName
    void $ string ": "
    job <- pJob
    pure (name, job)

opFunc :: Operation -> ((Int, Int) -> Int)
opFunc Add = uncurry (+)
opFunc Sub = uncurry (-)
opFunc Mul = uncurry (*)
opFunc Div = uncurry div

eval :: Map Text Job -> Text -> Int
eval ms m = case ms M.! m of
    Literal n -> n
    Result op xs -> opFunc op $ bimap (eval ms) (eval ms) xs

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day21.txt"
    let monkeys = M.fromList $ maybe (error "bad input") id $ parseMaybe (sepEndBy1 pMonkeyJob eol) contents
    print $ eval monkeys "root"
