{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isLower)
import Data.Functor ((<&>), void)
import Data.List (foldl', sort)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), eof, parseMaybe, sepEndBy1, some, takeWhile1P)
import Text.Megaparsec.Char (char, eol, string)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Destination
    = InDest !Text
    | OutDest
    | RootDest
    deriving Show

data Result
    = DirResult !Text
    | FileResult !Int !Text
    deriving Show

data Command
    = CdCmd !Destination
    | LsCmd !(Seq Result)
    deriving Show

destination :: Parser Destination
destination =
    (string ".." *> pure OutDest)
    <|> (char '/' *> pure RootDest)
    <|> (takeWhile1P (Just "dirname character") isLower <&> InDest)

result :: Parser Result
result =
    (string "dir " *> takeWhile1P (Just "dirname character") isLower <&> DirResult)
    <|> (do
        size <- L.decimal
        void $ char ' '
        name <- takeWhile1P (Just "filename character") (\c -> c == '.' || isLower c)
        pure $ FileResult size name)

command :: Parser Command
command = string "$ " *> (
    (string "cd " *> destination <* (void eol <|> eof) <&> CdCmd)
    <|> (string "ls" *> eol *> sepEndBy1 result eol <&> S.fromList <&> LsCmd))

input :: Parser [Command]
input = some command

data Directory = Directory
    { subdirs :: !(Map Text Directory)
    , files :: !(Map Text Int)
    } deriving Show

data Cursor = Cursor
    { current :: !Directory
    , _parentUpdaters :: ![Directory -> Directory]
    }

descend :: Text -> Cursor -> Cursor
descend subdirname (Cursor dir parents) = Cursor subdir (updater:parents)
  where
    subdir = M.findWithDefault (Directory M.empty M.empty) subdirname (subdirs dir)
    updater subdir' = dir { subdirs = M.insert subdirname subdir' (subdirs dir) }

ascend :: Cursor -> Cursor
ascend (Cursor _ []) = error "cannot ascend from root"
ascend (Cursor dir (p:parents)) = Cursor (p dir) parents

ascendToRoot :: Cursor -> Cursor
ascendToRoot cursor@(Cursor _ []) = cursor
ascendToRoot cursor = ascendToRoot $ ascend cursor

runCommand :: Cursor -> Command -> Cursor
runCommand cursor (CdCmd (InDest subdirname)) = descend subdirname cursor
runCommand cursor (CdCmd OutDest) = ascend cursor
runCommand cursor (CdCmd RootDest) = ascendToRoot cursor
runCommand cursor (LsCmd results) = cursor { current = addFiles (current cursor) results }

runCommands :: Foldable t => Cursor -> t Command -> Cursor
runCommands = foldl' runCommand

addFile :: Directory -> Result -> Directory
addFile dir (DirResult _) = dir
addFile dir (FileResult size filename) = dir { files = M.insert filename size (files dir) }

addFiles :: Foldable t => Directory -> t Result -> Directory
addFiles = foldl' addFile

totalSize :: Directory -> Int
totalSize (Directory subdirs files) = sum (map totalSize $ M.elems subdirs) + sum (M.elems files)

allSizes :: Directory -> [Int]
allSizes d@(Directory subdirs _) = totalSize d : concatMap allSizes subdirs

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day7.txt"
    let commands = maybe (error "bad input") id $ parseMaybe input contents
        root = Directory M.empty M.empty
        root' = current $ ascendToRoot $ runCommands (Cursor root []) commands
    print $ sum $ filter (<= 100000) $ allSizes root'
    let threshold = 30_000_000 - (70_000_000 - totalSize root')
    print $ head $ sort $ filter (>= threshold) $ allSizes root'
