{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import System.IO.Temp
import System.Posix.Process
import System.Process
import System.Exit

import Text.Printf


template = "/Users/scott/Code/hsl/template.hs"


data HSL = HSL { compile :: Bool
               , setup :: String
               , evaluate :: String
               } deriving (Show, Data, Typeable)


main :: IO ()
main = withSystemTempDirectory "hsl" $ \tmp -> do
    opt <- cmdArgs HSL { compile = def
                       , setup = def
                       , evaluate = def &= argPos 0
                       }

    let bin = tmp ++ "/Main"
        hs = bin ++ ".hs"

    tpl <- readFile template
    writeFile hs $ printf tpl (setup opt) (evaluate opt)

    if compile opt then runCompiled hs bin
                   else executeFile "runghc" True ["-isrc", hs] Nothing

  where
    runCompiled hs bin = do
        let ghc = "ghc -O2 -isrc " ++ hs
        (_, _, _, p) <- createProcess (shell ghc){ std_out = CreatePipe }
        ec <- waitForProcess p
        case ec of ExitSuccess -> executeFile bin False [] Nothing
                   ExitFailure c -> error ("Compilation failed with " ++ show c)
