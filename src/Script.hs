import Gimlh
import System.IO

main :: IO ()
main = do
    contents <- parseFile "../test/test_file.giml"
    print $ simplifyGiml contents
