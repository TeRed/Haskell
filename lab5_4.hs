-- import System.IO     
 
-- main = do     
--     contentsStr <- readFile "machine.txt"
--     excludeWordsStr <- readFile "exclude_words.txt"
--     let contents = words contentsStr
--         excludeWords = words excludeWordsStr
--         contains = \a -> elem a excludeWordsStr
--         parsedContent = unwords (filter contains (contents))
--     writeFile "new_machine.txt" parsedContent