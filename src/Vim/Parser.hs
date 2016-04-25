{-# LANGUAGE OverloadedStrings #-}
module Vim.Parser where

import Text.Parsec
import Text.Parsec.ByteString
import Data.ByteString.Char8 (ByteString, pack)
import Data.Map.Strict

import Vim.VimLine
import Vim.VimValue
import Vim.NeoBundle

-- $setup
-- >>> :set -XOverloadedStrings

neobundle :: Parsec ByteString u NeoBundle
neobundle = try neobundleLazy <|> nbsimple

nbsimple :: Parsec ByteString u NeoBundle
nbsimple = do
    _ <- spaces *> string "NeoBundle" <* spaces
    NeoBundle <$> quoted

neobundleLazy = do
    _ <- string "NeoBundleLazy" <* spaces
    bundleName <- quoted <* spaces <* char ',' <* spaces
    options <- fromVimDict <$> vimDict
    return $ NeoBundleLazy bundleName (fromMap options)
    where
        fromVimDict (VimDict kvp) = fromList kvp

vimValue :: Parsec ByteString u VimValue
vimValue = vimString <|> vimList <|> vimDict
-- |
-- >>> parseTest vimValue "{ 'autoload' : {'filetypes': ['haskell', 'coq']}}"
-- {"autoload" : {"filetypes" : ["haskell","coq"]}}

quoted :: Parsec ByteString u ByteString
quoted = do
    quote <- oneOf "\"'"
    pack <$> anyChar `manyTill` char quote

vimString :: Parsec ByteString u VimValue
vimString = VimString <$> quoted

-- |
-- >>> parseTest vimString "\"there \""
-- "there "
-- >>> parseTest vimString "' here '"
-- " here "

vimList :: Parsec ByteString u VimValue
vimList = do
    _ <- spaces *> char '[' <* spaces
    inner <- vimValue `sepBy` (spaces *> char ',' <* spaces)
    _ <- char ']' <* spaces
    return . VimList $ inner

-- |
-- >>> parseTest vimList "[ \"this\", [\"is\" , 'good'] ]"
-- ["this",["is","good"]]

kvPair :: Parsec ByteString u (ByteString, VimValue)
kvPair = do
    key <- quoted
    _ <- spaces *> char ':' <* spaces
    value <- vimValue
    return (key, value)

vimDict :: Parsec ByteString u VimValue
vimDict = do
    _ <- spaces *> char '{' <* spaces
    kvs <- kvPair `sepBy` (spaces *> char ',' <* spaces)
    _ <- char '}' <* spaces
    return . VimDict $ kvs
-- |
-- >>> parseTest vimDict "{ 'foo' : 'bar', 'baz' : ['a', 'b']}"
-- {"foo" : "bar", "baz" : ["a","b"]}

