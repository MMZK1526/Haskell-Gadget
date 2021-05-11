module Gadgets.IO where

import           Control.Monad (liftM2)
import           Control.Exception (handle, throw)
import           System.IO as IO 
  (Handle, hClose, hFlush, hGetLine, hIsEOF, stdin, stdout)
import           System.IO.Error
  ( IOErrorType, ioeGetFileName, illegalOperationErrorType, isDoesNotExistError
  , isEOFError, mkIOError
  )

import           Gadgets.Monad (void_)

-- | Reads all lines from @stdin@, returning a @[String]@.
--
getLines :: IO [String]
getLines = hGetLines stdin

-- | Reads all lines from a handle, returning a @[String]@.
--
hGetLines :: Handle -> IO [String]
hGetLines hdl = do
  b <- hIsEOF hdl
  if b 
    then return []
    else liftM2 (:) (hGetLine hdl) (hGetLines hdl)

-- | Specifically handles DNE exceptions.
--
handleDNE :: (IOError -> IO a) -> IO a -> IO a
handleDNE m 
  = handle $ \e -> if isDoesNotExistError e
    then m e
    else throw e

-- | Handles DNE exceptions with the file's full name (with path).
--
handleDNEPath :: (Maybe FilePath -> IO a) -> IO a -> IO a
handleDNEPath = handleDNE . (. ioeGetFileName)

-- | Does nothing on DNE exceptions.
-- 
handleDNE_ :: IO () -> IO ()
handleDNE_ = handleDNE $ const void_

-- | Specifically handles EOF exceptions.
--
handleEOF :: (IOError -> IO a) -> IO a -> IO a
handleEOF m 
  = handle $ \e -> if isEOFError e
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

-- | Strictly outputs a @Char@ via @stdout@.
--      
putChar' :: Char -> IO ()
putChar' ch
  = putChar ch >> hFlush stdout

-- | Outputs a new line.
-- 
putLn :: IO ()
putLn = putStrLn ""

-- | Making and throwing an @IOError@.
--
throwIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> a
throwIOError t s h p = throw $ mkIOError t s h p

-- | Making and throwing an @IOError@ without file path and handle.
--
throwIOError_ :: IOErrorType -> String -> a
throwIOError_ t s = throwIOError t s Nothing Nothing
