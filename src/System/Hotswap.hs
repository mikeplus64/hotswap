module System.Hotswap (newPlugin, Plugin (..), usePlugin, reloadPlugin) where

import System.Plugins.Load
import Data.IORef 

data Plugin a = Plugin {
    pluginObject       :: FilePath,    -- ^ Path to object
    pluginIncludes     :: [FilePath],  -- ^ Include paths.
    pluginFunctionName :: String,      -- ^ Name of the symbol to find.
    pluginFunction     :: IORef a,     -- ^ Loaded function.
    pluginModule       :: IORef Module -- ^ Loaded module.
}

-- | 'usePlugin' provides a simple way to use plugins of type 'Plugin (a -> b)', ie, only 
-- single argumented ones.
usePlugin :: Plugin (a -> b) -> a -> IO b
usePlugin p x = fmap ($ x) $ readIORef $ pluginFunction p

-- | Create a new plugin. Don't use this to reload plugins.
newPlugin :: FilePath -> [FilePath] -> String -> IO (Plugin a)
newPlugin obj incs name = do
    plugin <- load_ obj incs name

    case plugin of

        LoadSuccess m f -> do
            fref <- newIORef f
            mref <- newIORef m
            return $! Plugin obj incs name fref mref

        _ -> error "no such module or function"

-- | Reload a plugin in-place.
reloadPlugin :: Plugin a -> IO ()
reloadPlugin (Plugin _ _ name fref mref) = do 
    m <- readIORef mref
    plugin <- reload m name
    case plugin of
        LoadSuccess newm newf -> do
            writeIORef mref newm
            writeIORef fref newf
        _ -> error "no such module or function"

