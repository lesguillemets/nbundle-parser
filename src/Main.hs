import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Vim.VimLine as VL

main = do
    home <- getHomeDirectory
    BC.readFile (home </> ".vimrc") >>=
        mapM_ (BC.putStrLn . VL.lineContent) . VL.toVimLines
