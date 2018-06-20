import System.IO     
 
main = do     
    contentsStr <- readFile "test_files/machine.txt"
    excludeWordsStr <- readFile "test_files/exclude_words.txt"
    let contents = words contentsStr
        excludeWords = words excludeWordsStr
        notContains w = length (filter (== w) excludeWords) == 0
        parsedContent = unwords $ filter notContains contents
    writeFile "test_files/new_machine.txt" parsedContent