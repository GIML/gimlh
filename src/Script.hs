import Gimlh
import System.IO

main :: IO ()
main = do
    -- handle <- openFile "../test/test_file.giml" ReadMode
    -- contents <- hGetContents handle
    let contents = "# shit yeah\n:num: shitNum\n1234.432\n\n:list: someList\n# shit shit\n1, 2, 3, 4\n5, 6, 7, 8\n\n:num: someNum\n1234\n\n:text: someText\ntext text text\ntext text\n"
    let parsed = parseLines (lines contents) Nothing
    -- hClose handle
    print parsed
