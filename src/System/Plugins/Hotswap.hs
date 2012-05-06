module System.Plugins.Hotswap (
    Plugin (..), 
    newPlugin,
    usePlugin, 
    usePluginIO,
    runPlugin,
    reloadPlugin, 
    readPlugin, 
    withPlugin,
    withPluginIO,
    putPlugin
) where

import System.Plugins.Load
import Data.IORef

data Plugin a = Plugin {
    pluginObject       :: FilePath,    -- ^ Path to object
    pluginIncludes     :: [FilePath],  -- ^ Include paths.
    pluginDataName     :: String,      -- ^ Name of the symbol to find.
    pluginData         :: IORef a,     -- ^ Loaded data.
    pluginModule       :: IORef Module -- ^ Loaded module.
}

-- | 'usePlugin' provides a simple way to use plugins of type 'Plugin (a -> b)', ie, only 
-- single argumented ones.
usePlugin :: Plugin (a -> b) -> a -> IO b
usePlugin plug x = fmap ($ x) (readPlugin plug)

-- | 'usePlugin' for plugins returning IO.
usePluginIO :: Plugin (a -> IO b) -> a -> IO b
usePluginIO plug x = readPlugin plug >>= ($ x)

-- | 'runPlugin' runs an 'IO a' returning 'Plugin'.
runPlugin :: Plugin (IO ()) -> IO ()
runPlugin plug = readPlugin plug >>= id

-- | 'withPlugin' provides a way to run a function on a plugin, modifying the plugin in-place.
withPlugin :: Plugin a -> (a -> a) -> IO ()
withPlugin plug f = do
    pd <- readPlugin plug 
    putPlugin plug (f pd)

-- | 'withPlugin' for functions returning IO.
withPluginIO :: Plugin a -> (a -> IO a) -> IO ()
withPluginIO plug f = do
    pd <- readPlugin plug
    r  <- f pd
    putPlugin plug r

-- | Create a new plugin, expecting a type. Don't use this to reload plugins.
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

-- | Read the 'pluginData' 'IORef'.
readPlugin :: Plugin a -> IO a
readPlugin = readIORef . pluginData

-- | Replace the contents of the 'pluginData'.
putPlugin :: Plugin a -> a -> IO ()
putPlugin = writeIORef . pluginData

{-
data Hotswap a = Hotswap {
    hotswapThread :: IORef (Maybe ThreadId),
    hotswapPlugin :: Plugin a -- something seems wrong about passing this around all the time, but alas...
}

-- | Automatically poll the filesystem for changes in the specified 'Plugin''s object file, and
-- reload the plugin if necessary.
newHotswap :: Plugin a -> IO (Hotswap a)
newHotswap plug = do
    ht  <- forkHotswapThread plug
    ref <- newIORef (Just ht)
    return $ Hotswap ref plug

-- | 'newHotswap', only returning the ThreadId.
forkHotswapThread :: Plugin a -> IO ThreadId
forkHotswapThread plug = do
    md5 <- md5File $ pluginObject plug
    forkIO (loop md5)
  where
    md5File  = fmap md5sum . B.readFile

    loop old = do
        threadDelay 100000
        new <- md5File $ pluginObject plug
        if new == old
            then loop new
            else do
                reloadPlugin plug
                loop new

-- | Restart a 'Hotswap' from one that was killed, otherwise, do nothing.
restartHotswap :: Hotswap a -> IO ()
restartHotswap (Hotswap h p) = do
    r <- readIORef h

    case r of
        Nothing -> do
            thread <- forkHotswapThread p
            writeIORef h (Just thread)

        _ -> return ()

killHotswap :: Hotswap a -> IO ()
killHotswap (Hotswap h _) = do
    r <- readIORef h

    case r of 
        Just thread -> do
            killThread thread
            writeIORef h Nothing
        _ -> return ()
-}
