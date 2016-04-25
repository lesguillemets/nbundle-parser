{-# LANGUAGE OverloadedStrings #-}
module Vim.VimValue where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
import Data.List (intercalate)
import Data.Monoid
-- $setup
-- >>> :set -XOverloadedStrings

data VimValue = VimString ByteString
              | VimList [VimValue]
              | VimDict [(ByteString, VimValue)]

instance Show VimValue where
    show (VimString s) = show s
    show (VimList es) = show es
    show (VimDict kvs) = "{" <> (intercalate ", " . map showKV) kvs <> "}"
        where
            showKV :: (ByteString, VimValue) -> String
            showKV (key, value)
                = mconcat [ show key
                          , " : "
                          , show value]

-- |
-- >>> let fts = VimList . map VimString $ ["haskell", "python"]
-- >>> let cmds = VimList . map VimString $ ["LoadMe"]
-- >>> VimDict [("autoload", VimDict [("filetypes", fts), ("commands",cmds)])]
-- {"autoload" : {"filetypes" : ["haskell","python"], "commands" : ["LoadMe"]}}
