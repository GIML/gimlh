import Gimlh
import System.IO

main :: IO ()
main = do
    contents <- readFile "../test/test_file.giml"
    let content = lines contents
        parsed = parseLines content Nothing
    print parsed
