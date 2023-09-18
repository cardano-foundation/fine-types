import Control.Monad
import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.LocalBuildInfo
import System.Directory
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks{postConf = \_ _ _ lbi -> generateSources lbi}

generateSources :: LocalBuildInfo -> IO ()
generateSources lbi =
    forM_ (allComponentsInBuildOrder lbi) $ \clbi -> do
        let dir = autogenComponentModulesDir lbi clbi
            file = dir </> "Language/FineTypes/Test/UTxO.hs"
        putStrLn $ concat ["*** Creating ", file, "."]
        createDirectoryIfMissing True (takeDirectory file)
        callProcess
            "fine-types"
            [ "convert"
            , "haskell"
            , "-i"
            , "test/data/HaskellUTxO.fine"
            , "-o"
            , file
            ]
