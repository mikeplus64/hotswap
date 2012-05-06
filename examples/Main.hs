import Control.Monad
import System.Plugins.Hotswap

main :: IO ()
main = do
    inputHandler <- newPlugin "Plugin.o" [] "inputHandler" :: IO (Plugin (String -> String))
    hotswap      <- newHotswap inputHandler

    forever $ do
        l <- getLine
        case l of
            "killHotswap"    -> killHotswap hotswap
            "restartHotswap" -> reloadPlugin inputHandler >> restartHotswap hotswap
            _                -> putStrLn =<< usePlugin inputHandler l
