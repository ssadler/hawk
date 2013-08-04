
module HSL.IO (runLineProcessor, unsafePutErr) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO (isEOF)
import           System.IO.Unsafe (unsafeInterleaveIO)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Unsafe (unsafePerformIO)


import HSL.Types


runLineProcessor :: Renderable a => ([T.Text] -> a) -> IO ()
runLineProcessor f = lazyLines >>= mapM_ TIO.putStrLn . render . f

lazyLines = do eof <- isEOF
               if eof then return []
                      else do line <- TIO.getLine
                              rest <- unsafeInterleaveIO lazyLines
                              return (line:rest)


unsafePutErr :: String -> a -> a
unsafePutErr e a = unsafePerformIO $ hPutStrLn stderr e >> return a
