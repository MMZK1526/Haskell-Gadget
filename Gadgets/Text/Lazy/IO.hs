module Gadgets.Text.IO where

import           Control.Monad (liftM2)
import           System.IO (IOMode, Handle)
import qualified System.IO as IO

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

type Text = T.Text

-- | Reads all lines from @stdin@, returning a @[Text]@.
--
getLines' :: IO [Text]
getLines' = hGetLines' IO.stdin

-- | Reads all lines from a handle, returning a @[Text]@.
--
hGetLines' :: Handle -> IO [Text]
hGetLines' hdl = do
  b <- IO.hIsEOF hdl
  if b 
    then return []
    else liftM2 (:) (T.hGetLine hdl) (hGetLines' hdl)

-- | Strictly outputs a @Text@ via @stdout@.
--      
putStr :: Text -> IO ()
putStr str 
  = T.putStr str >> IO.hFlush IO.stdout

-- | Similar to @withFile@ but takes a @Text@ as file path.
--
withFile :: Text -> IOMode -> (Handle -> IO a) -> IO a
withFile = IO.withFile . T.unpack
