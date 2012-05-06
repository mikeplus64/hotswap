import Control.Monad
import System.Plugins.Hotswap

main :: IO ()
main = do
    inputHandler <- newPlugin "Plugin.o" [] "inputHandler" :: IO (Plugin (IO Bool))
    forever $ do
        r <- runPlugin inputHandler
        when r $ reloadPlugin inputHandler
