{-# LANGUAGE OverloadedStrings,FlexibleInstances,UndecidableInstances,OverlappingInstances #-}

module HSL.Types where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intersperse)
import Data.Int (Int64)
import Data.Maybe (fromJust)


-- Type witnesses

s :: B.ByteString
s = undefined

i :: Int
i = undefined

f :: Float
f = undefined


-- Marshalling

class Datum a where
    bs :: a -> B.ByteString
    parse :: B.ByteString -> a
    parse = undefined
    parseMany :: [B.ByteString] -> a
    parseMany = parse . head

instance Datum Int where
    bs = B.pack . show
    parse = fst . fromJust . B.readInt

instance Datum Int64 where
    bs = B.pack . show

instance Datum Char where
    bs = B.pack . show
    parse = B.head

instance Datum String where
    bs = B.pack

instance Datum B.ByteString where
    bs = id
    parse = id


instance (Datum a, Datum b) => Datum (a, b) where
    bs (a, b) = B.concat $ intersperse "\t" [bs a, bs b]
    parseMany [a, b] = (parse a, parse b)

instance (Datum a, Datum b, Datum c) => Datum (a, b, c) where
    bs (a, b, c) = B.concat $ intersperse "\t" [bs a, bs b, bs c]
    parseMany [a, b, c] = (parse a, parse b, parse c)

instance (Datum a, Datum b, Datum c, Datum d) => Datum (a, b, c, d) where
    bs (a, b, c, d) = B.concat $ intersperse "\t" [bs a, bs b, bs c, bs d]
    parseMany [a, b, c, d] = (parse a, parse b, parse c, parse d)

instance (Datum a, Datum b, Datum c, Datum d, Datum e) => Datum (a, b, c, d, e) where
    bs (a, b, c, d, e) = B.concat $ intersperse "\t" [bs a, bs b, bs c, bs d, bs e]
    parseMany [a, b, c, d, e] = (parse a, parse b, parse c, parse d, parse e)


class Renderable a where
    render :: a -> [B.ByteString]

instance (Datum a) => Renderable a where
    render = (:[]) . bs

instance (Datum a) => Renderable [a] where
    render = map bs

instance (Datum a) => Renderable [[a]] where
    render = map bs . concat
