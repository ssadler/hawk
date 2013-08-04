{-# LANGUAGE OverloadedStrings,FlexibleInstances,UndecidableInstances,OverlappingInstances #-}

module HSL.Types where

import           Data.Int (Int64)
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import qualified Data.Text as T


-- Type witnesses

t :: T.Text
t = undefined

i :: Int
i = undefined

f :: Float
f = undefined


-- Marshalling

class Datum a where
    bs :: a -> T.Text
    parse :: T.Text -> a
    parse = undefined
    parseMany :: [T.Text] -> a
    parseMany = parse . head

instance Datum Int where
    bs = T.pack . show

instance Datum Int64 where
    bs = T.pack . show

instance Datum Char where
    bs = T.pack . show
    parse = T.head

instance Datum String where
    bs = T.pack

instance Datum T.Text where
    bs = id
    parse = id


instance (Datum a, Datum b) => Datum (a, b) where
    bs (a, b) = T.concat $ intersperse "\t" [bs a, bs b]
    parseMany [a, b] = (parse a, parse b)

instance (Datum a, Datum b, Datum c) => Datum (a, b, c) where
    bs (a, b, c) = T.concat $ intersperse "\t" [bs a, bs b, bs c]
    parseMany [a, b, c] = (parse a, parse b, parse c)

instance (Datum a, Datum b, Datum c, Datum d) => Datum (a, b, c, d) where
    bs (a, b, c, d) = T.concat $ intersperse "\t" [bs a, bs b, bs c, bs d]
    parseMany [a, b, c, d] = (parse a, parse b, parse c, parse d)

instance (Datum a, Datum b, Datum c, Datum d, Datum e) => Datum (a, b, c, d, e) where
    bs (a, b, c, d, e) = T.concat $ intersperse "\t" [bs a, bs b, bs c, bs d, bs e]
    parseMany [a, b, c, d, e] = (parse a, parse b, parse c, parse d, parse e)


class Renderable a where
    render :: a -> [T.Text]

instance (Datum a) => Renderable a where
    render = (:[]) . bs

instance (Datum a) => Renderable [a] where
    render = map bs

instance (Datum a) => Renderable [[a]] where
    render = map bs . concat
