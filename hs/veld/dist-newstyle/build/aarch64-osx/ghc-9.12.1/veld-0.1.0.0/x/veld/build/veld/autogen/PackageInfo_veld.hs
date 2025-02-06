{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_veld (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "veld"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "a language full of goodies"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
