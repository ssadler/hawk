{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import System.IO.Temp
import System.Posix.Process

import Text.Printf


template = "/Users/scott/Code/hsl/template.hs"


data HSL = HSL { interpreted :: Bool
               , setup :: String
               , evaluate :: String
               } deriving (Show, Data, Typeable)


main :: IO ()
main = withSystemTempDirectory "hsl" $ \tmp -> do
    args <- cmdArgs HSL { interpreted = def
                        , setup = def
                        , evaluate = def &= argPos 0
                        }
    tpl <- readFile template
    let binhs = tmp ++ "/Main.hs"
    writeFile binhs $ printf tpl (setup args) (evaluate args)
    executeFile "runghc" True ["-isrc", binhs] Nothing



