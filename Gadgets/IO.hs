{-# LANGUAGE OverloadedStrings #-}

module Gadgets.IO where

import           Control.Exception (handle, throw)
import           Data.Maybe (fromJust)
import           Data.String (IsString(..))
import           System.IO as IO (hFlush, stdout)
import           System.IO.Error 
  (mkIOError, eofErrorType, illegalOperationErrorType, ioeGetErrorType)
import           System.Info (os)
import           System.Environment (lookupEnv)

import qualified Data.Text as T (Text)
import qualified Data.Text.IO as TIO (putStr, putStrLn)

import           Gadgets.Monad (void_)

type Text = T.Text

-- | Specifically handles EOF exceptions.
--
handleEOF :: (IOError -> IO a) -> IO a -> IO a
handleEOF m 
  = handle $ \e -> if ioeGetErrorType e == eofErrorType 
    then m e
    else throw e

-- | Does nothing on EOF exceptions.
-- 
handleEOF_ :: IO () -> IO ()
handleEOF_ = handleEOF $ const void_

-- | Make a new line on EOF exceptions.
-- 
handleEOFLn_ :: IO () -> IO ()
handleEOFLn_ = handleEOF $ const putLn

{-# SPECIALISE homePath :: IO Text #-}
-- | Get the $HOME variable or its equivalence across platforms.
-- Only supports OS X, Windows & Linux.
-- Throws an IllegalOperationError in the case of unsupported OS.
-- 
homePath :: IsString a => IO a
homePath = fmap (fromString . fromJust) $ lookupEnv $ case os of
  "darwin"  -> "HOME"
  "linux"   -> "HOME"
  "mingw32" -> "HOMEPATH"
  _         -> throw $ mkIOError 
               illegalOperationErrorType 
               ("This operating system (" ++ os ++ ") is not supported") 
               Nothing 
               Nothing

-- | Strictly outputs a @Char@ via @stdout@.
--      
putChar' :: Char -> IO ()
putChar' ch
  = putChar ch >> hFlush stdout

-- | Outputs a new line.
-- 
putLn :: IO ()
putLn = TIO.putStrLn ""

-- | Strictly outputs a @Text@ via @stdout@.
--      
putStr' :: Text -> IO ()
putStr' str 
  = TIO.putStr str >> hFlush stdout
