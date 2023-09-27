import Distribution.PackageDescription (mkUnqualComponentName)
import Distribution.Simple
    ( UserHooks (..)
    , defaultMainWithHooks
    , simpleUserHooks
    )
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Language.FineTypes.Commands.Convert.Haskell.Cabal
    ( addFineTypes
    , stdoutTracer
    )

main :: IO ()
main =
    defaultMainWithHooks
        $ addFineTypes
            stdoutTracer
            simpleUserHooks
            "test/data"
