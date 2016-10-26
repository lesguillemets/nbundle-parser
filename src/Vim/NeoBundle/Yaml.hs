{-# LANGUAGE OverloadedStrings #-}
module Vim.NeoBundle.Yaml
    (
        toYamlEntry
    )
    where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
import Data.Monoid

import Vim.NeoBundle
import Vim.VimValue

shiftWidth :: Int
shiftWidth = 2
indent :: Int -> ByteString -> ByteString
indent n = (BC.replicate (n*shiftWidth) ' ' <>)

indents :: Int -> [ByteString] -> [ByteString]
indents = map . indent

itemize :: [ByteString] -> [ByteString]
itemize = map ("- " <>)

class Yamlable a where
    toYamlEntry :: a -> [ByteString]

instance Yamlable NeoBundle where
    toYamlEntry (NeoBundle bname) =
        [bname <> ": {}"]
    toYamlEntry (NeoBundleLazy bName bOption) =
        (bName <> ":") : indents 1 (toYamlEntry bOption)

instance Yamlable BundleOptions where
    toYamlEntry (BundleOptions au) = toYamlEntry au

instance Yamlable AutoLoad where
    toYamlEntry (AutoLoad auCmd auFt) =
        toYGs "command:" auCmd <> toYGs "filetype:" auFt
        where
            toYGs :: ByteString -> [String] -> [ByteString]
            toYGs p = toYG p . map BC.pack
            toYG :: ByteString -> [ByteString] -> [ByteString]
            toYG _ [] = []
            toYG pre [x] = [pre <> " " <> x]
            toYG pre xs = pre : (indents 1 . itemize $ xs)
