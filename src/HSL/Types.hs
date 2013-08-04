{-# LANGUAGE OverloadedStrings,FlexibleInstances,UndecidableInstances,OverlappingInstances #-}

module HSL.Types where

import qualified Data.Text as T
import Data.List (intersperse)
import Data.Int (Int64)


-- Type witnesses

t :: T.Text
t = undefined

i :: Int
i = undefined

f :: Float
f = undefined


-- Output Rendering

class Datum a where
    bs :: a -> T.Text

class Renderable a where
    render :: a -> [T.Text]


instance Datum Int where bs = T.pack . show
instance Datum Int64 where bs = T.pack . show
instance Datum Char where bs = T.pack . show
instance Datum String where bs = T.pack
instance Datum T.Text where bs = id

instance (Datum a, Datum b) => Datum (a, b) where
    bs (a, b) = T.concat $ intersperse "\t" [bs a, bs b]
instance (Datum a, Datum b, Datum c) => Datum (a, b, c) where
    bs (a, b, c) = T.concat $ intersperse "\t" [bs a, bs b, bs c]
instance (Datum a, Datum b, Datum c, Datum d) => Datum (a, b, c, d) where
    bs (a, b, c, d) = T.concat $ intersperse "\t" [bs a, bs b, bs c, bs d]
instance (Datum a, Datum b, Datum c, Datum d, Datum e) => Datum (a, b, c, d, e) where
    bs (a, b, c, d, e) = T.concat $ intersperse "\t" [bs a, bs b, bs c, bs d, bs e]

instance (Datum a) => Renderable a where
    render = (:[]) . bs
instance (Datum a) => Renderable [a] where
    render = map bs
instance (Datum a) => Renderable [[a]] where
    render = map bs . concat
