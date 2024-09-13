{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ex1 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ex1"
version :: Version
version = Version [1,0,0,0] []

synopsis :: String
synopsis = "CSU34016 Exercise One"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
