import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as BC

main = do
    putStrLn "hello, world!"
    BC.putStrLn . BC.pack $ "there"

