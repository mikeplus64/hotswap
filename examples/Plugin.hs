module Plugin where
import System.Random (randomRIO)
import System.IO     (hSetBuffering, stdout, BufferMode (NoBuffering))

inputHandler :: IO Bool
inputHandler = do
    hSetBuffering stdout NoBuffering
    putStrLn "Guess a number between 0 and 10. Guess -1 to reload the plugin."
    g <- fmap read getLine :: IO Int

    if g == -1
        then do
            putStrLn "Reloading ..."
            return True
        else do
            r <- randomRIO (0, 10) :: IO Int
            if g == r
                then putStrLn "Congratulations! You win nothing!"
                else putStrLn "HAHA, YOU'RE HORRIBLE"
            return False
