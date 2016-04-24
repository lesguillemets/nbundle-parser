{-# LANGUAGE OverloadedStrings #-}
module Vim where
import Data.List
import Data.Char (isSpace)
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString, dropWhile)

type Input = ByteString
type IndentLevel = Int
data RawLine = RawLine {rawIndent :: IndentLevel, rawContent :: Input}
             | Continued {rawContent :: Input}
instance Show RawLine where
    show (RawLine i r) = "RawLine " ++ show i ++ " " ++ show r
    show (Continued r) = "Continued " ++ show r
isCont :: RawLine -> Bool
isCont (RawLine _ _) = False
isCont (Continued _) = True

data VimLine = VimLine {
    indentLevel :: IndentLevel,
    lineContent :: Input
}

instance Show VimLine where
    show (VimLine i r) = "Line " ++ show i ++ " " ++ show r

toVimLines :: Input -> [VimLine]
toVimLines = folder . map rawLine . BC.lines
    where
        folder :: [RawLine] -> [VimLine]
        folder []     = []
        folder (h:rs) = let (currentLine, rest) = span isCont rs
                            in
                            VimLine
                                (rawIndent h)
                                (BC.unwords . map rawContent $ h:currentLine)
                                    : folder rest


rawLine :: Input -> RawLine
rawLine l = let (thisIndent, body) = BC.span isSpace l
                in
                case BC.uncons body of
                     Just ('\\', r) -> Continued (BC.dropWhile isSpace r)
                     _ -> RawLine (BC.length thisIndent) body

