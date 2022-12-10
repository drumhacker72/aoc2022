import Data.Functor ((<&>))
import Data.List.Split (chunksOf)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepEndBy1)
import Text.Megaparsec.Char (eol, string)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Instruction
    = Addx Int
    | Noop
    deriving Show

addxInstruction, noopInstruction, instruction :: Parser Instruction
addxInstruction = string "addx " *> L.signed (pure ()) L.decimal <&> Addx
noopInstruction = string "noop" >> pure Noop
instruction = addxInstruction <|> noopInstruction

genSignal :: Int -> [Instruction] -> [Int]
genSignal x [] = repeat x
genSignal x (Addx v : rest) = let x' = x + v in x : x' : genSignal x' rest
genSignal x (Noop : rest) = x : genSignal x rest

pixel :: Int -> Int -> Char
pixel pos sig = if abs (pos - sig) <= 1 then '#' else '.'

main :: IO ()
main = do
    contents <- readFile "day10.txt"
    let insts = maybe (error "bad input") id $ parseMaybe (sepEndBy1 instruction eol) contents
    let signal = 1 : genSignal 1 insts

    let strengths = zipWith (*) [1..] signal
    print $ sum $ map (\i -> strengths !! (i - 1)) $ [20, 60, 100, 140, 180, 220]

    let crt = map (zipWith pixel [0..39]) $ take 6 $ chunksOf 40 signal
    mapM_ putStrLn crt
