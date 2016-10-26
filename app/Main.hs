{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString, isPrefixOf)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Data.Either (rights)
import Text.Parsec

import Vim.VimLine as VL
import Vim.Parser
import Vim.NeoBundle.Yaml

isBundle :: VimLine -> Bool
isBundle (VimLine _ c) = "NeoBundle" `isPrefixOf` c

main = do
    home <- getHomeDirectory
    BC.readFile (home </> ".vimrc") >>=
        mapM_ (BC.putStrLn . BC.unlines . toYamlEntry)
            . rights . map (parse neobundle "" . lineContent)
            . filter isBundle . VL.toVimLines
