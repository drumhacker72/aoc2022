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

data Job2 = Literal2 !Int | Result2 !Operation !(Text, Text) | X2
    deriving Show

upgrade :: Job -> Job2
upgrade (Literal a) = Literal2 a
upgrade (Result op xs) = Result2 op xs

data Expr
    = LitExpr !Int
    | AddExpr !Expr !Expr
    | SubExpr !Expr !Expr
    | MulExpr !Expr !Expr
    | DivExpr !Expr !Expr
    | XExpr
    deriving Show

addExpr, subExpr, mulExpr, divExpr :: Expr -> Expr -> Expr
addExpr (LitExpr a) (LitExpr b) = LitExpr (a + b)
addExpr a b = AddExpr a b
subExpr (LitExpr a) (LitExpr b) = LitExpr (a - b)
subExpr a b = SubExpr a b
mulExpr (LitExpr a) (LitExpr b) = LitExpr (a * b)
mulExpr a b = MulExpr a b
divExpr (LitExpr a) (LitExpr b) = LitExpr (a `div` b)
divExpr a b = DivExpr a b

build :: Map Text Job2 -> Text -> Expr
build ms m = case ms M.! m of
    Literal2 n -> LitExpr n
    Result2 Add (a, b) -> addExpr (build ms a) (build ms b)
    Result2 Sub (a, b) -> subExpr (build ms a) (build ms b)
    Result2 Mul (a, b) -> mulExpr (build ms a) (build ms b)
    Result2 Div (a, b) -> divExpr (build ms a) (build ms b)
    X2 -> XExpr

solve :: Expr -> Expr -> Int
solve (LitExpr _) (LitExpr _) = error "unexpected"
solve a@(LitExpr _) b = solve b a
solve XExpr (LitExpr z) = z
solve (AddExpr (LitExpr a) b) (LitExpr z) = solve b (LitExpr $ z - a)
solve (AddExpr a (LitExpr b)) (LitExpr z) = solve a (LitExpr $ z - b)
solve (SubExpr (LitExpr a) b) (LitExpr z) = solve b (LitExpr $ a - z)
solve (SubExpr a (LitExpr b)) (LitExpr z) = solve a (LitExpr $ z + b)
solve (MulExpr (LitExpr a) b) (LitExpr z) = solve b (LitExpr $ z `div` a)
solve (MulExpr a (LitExpr b)) (LitExpr z) = solve a (LitExpr $ z `div` b)
solve (DivExpr (LitExpr a) b) (LitExpr z) = solve b (LitExpr $ a `div` z)
solve (DivExpr a (LitExpr b)) (LitExpr z) = solve a (LitExpr $ b * z)
solve _ _ = error "unexpected"

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day21.txt"
    let monkeys = M.fromList $ maybe (error "bad input") id $ parseMaybe (sepEndBy1 pMonkeyJob eol) contents
    print $ eval monkeys "root"

    let monkeys2 = M.mapWithKey (\m j -> if m == "humn" then X2 else upgrade j) monkeys
        (left, right) = case monkeys2 M.! "root" of
            (Result2 _ xs) -> xs
            _ -> error "expected root to listen for 2 numbers"
        e1 = build monkeys2 left
        e2 = build monkeys2 right
    print $ solve e1 e2
