{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.FineTypes
    ( FineJSON
    ) where

import Prelude

import Data.Proxy
    ( Proxy (..)
    )
import Language.FineTypes.Export.Haskell.Value.Runtime
    ( FromValue (..)
    , ToTyp (..)
    , ToValue (..)
    )
import Language.FineTypes.Export.OpenAPI.Value.FromJSON
    ( valueFromJson
    )
import Language.FineTypes.Export.OpenAPI.Value.ToJSON
    ( jsonFromValue
    )
import Servant.API
    ( Accept (..)
    , JSON
    , MimeRender (..)
    , MimeUnrender (..)
    )

import Data.Aeson as JS

{-----------------------------------------------------------------------------
    Servant utilities
------------------------------------------------------------------------------}

-- | JSON data, but generated from a 'Typ'.
data FineJSON

instance Accept FineJSON where
    contentType _ = contentType (Proxy :: Proxy JSON)

instance forall a. ToValue a => MimeRender FineJSON a where
    mimeRender _ =
        JS.encode . jsonFromValue typ . toValue
      where
        typ = toTyp (Proxy :: Proxy a)

instance forall a. FromValue a => MimeUnrender FineJSON a where
    mimeUnrender _ bs =
        fmap fromValue . valueFromJson typ =<< JS.eitherDecode bs
      where
        typ = toTyp (Proxy :: Proxy a)
