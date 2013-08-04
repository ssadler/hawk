{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding (break)

import           Control.Category ((>>>))
import qualified Data.Text as T
import           Data.Char
import           Data.List hiding (lines, unlines, break)
import           Data.Maybe
import qualified Data.Map as Map
import           Data.Ord
import           Data.Tuple

import           HSL.Json
import           HSL.Stdlib
import           HSL.IO
import           HSL.Types


%s

p = %s

main = runLineProcessor p

