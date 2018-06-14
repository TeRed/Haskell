import System.Random

main :: IO ()

main = do
    num <- randomIO :: IO Int
    let guess = num `mod` 11
    putStr "Number to guess: \n"
    print guess
    putStrLn "Guess a number between 0 and 10"
    -- First
    putStrLn "Try (1/3):"
    line <- getLine
    let ans = read line :: Int
    if ans == guess then do
        putStrLn "Great!"
        return ()
    else do
    -- Second
    putStrLn "Try (2/3):"
    line <- getLine
    let ans = read line :: Int
    if ans == guess then do
        putStrLn "Great!"
        return ()
    else do
    -- Third
    putStrLn "Try (3/3):"
    line <- getLine
    let ans = read line :: Int
    if ans == guess then do
        putStrLn "Great!"
        return ()
    else do
        putStrLn "Sorry :("
        return ()