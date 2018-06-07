main :: IO ()

main = do
    putStrLn "Write some string (empty for end):"
    str <- getLine
    if (length str == 0)
        then return()
    putStrLn $ reverse str
    main