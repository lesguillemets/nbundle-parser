{-# LANGUAGE OverloadedStrings #-}
module Vim.NeoBundle where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Data.Map.Strict (Map, findWithDefault, fromList)
import Vim.VimValue

type BundleName = ByteString
data NeoBundle = NeoBundle BundleName
               | NeoBundleLazy BundleName BundleOptions

data BundleOptions = BundleOptions {
    _autoLoad :: AutoLoad
}
fromMap:: Map ByteString VimValue -> BundleOptions
fromMap m =
    let lookupDict = findWithDefault (VimDict [])
        lookupList = findWithDefault (VimList [])
        VimDict al =  lookupDict "autoload" m
        autoload = fromList al
        -- FIXME : here be dragons
        unstring (VimString x) = BC.unpack x
        unlist (VimList xs) = xs
        cmds = map unstring . unlist $ lookupList "commands" autoload
        fts = map unstring . unlist $ lookupList "filetypes" autoload
        in
            BundleOptions (AutoLoad cmds fts)


data AutoLoad = AutoLoad {_auCmds :: [String], _auFTs :: [String]}


instance Show NeoBundle where
    show (NeoBundle b) = "NeoBundle " <> show b
    show (NeoBundleLazy b s) = "NeoBundleLazy " <> show b <> ", " <> show s

instance Show BundleOptions where
    show (BundleOptions au) =
        mconcat [ "{'autoload' : "
                , show au
                , "}"]

-- |
-- >>> BundleOptions (AutoLoad [] ["haskell"])
-- {'autoload' : {'commands' : [], 'filetypes' : ["haskell"]}}

instance Show AutoLoad where
    show (AutoLoad cmd ft) =
        mconcat ["{'commands' : "
               , show cmd
               , ", 'filetypes' : "
               , show ft
               , "}"]
-- |
-- >>> AutoLoad ["Loremipsum"] []
-- {'commands' : ["Loremipsum"], 'filetypes' : []}
-- >>> AutoLoad [] ["haskell", "python"]
-- {'commands' : [], 'filetypes' : ["haskell","python"]}
