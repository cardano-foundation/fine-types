import Distribution.Simple
    ( UserHooks (..)
    , defaultMainWithHooks
    , simpleUserHooks
    )
import Distribution.Simple.PreProcess
    ( knownSuffixHandlers
    )
import Language.FineTypes.Commands.Convert.Haskell.Cabal
    ( fineTypes
    )

main :: IO ()
main =
    defaultMainWithHooks
        $ simpleUserHooks
            { hookedPreProcessors =
                knownSuffixHandlers <> [fineTypes]
            }
