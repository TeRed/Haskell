import System.IO
import Data.Char

main = do
    contents <- readFile "machine.txt"
    let contentsCaps = map toUpper contents
    writeFile "new_machine.txt" contentsCaps