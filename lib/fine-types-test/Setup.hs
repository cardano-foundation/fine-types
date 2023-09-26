import Distribution.Simple
    ( UserHooks (..)
    , defaultMainWithHooks
    , simpleUserHooks
    )
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Language.FineTypes.Commands.Convert.Haskell.Cabal (generateSources)

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks{postConf = \_ _ _ lbi -> generateSources lbi}
