module Gadgets.IO where

import           Control.Monad (liftM2)
import           Control.Exception (handle, throw)
import           Data.Maybe (fromJust)
import           Data.String (IsString(..))
import           System.IO as IO 
  ( IOMode, Handle, hClose, hFlush, hGetLine, hIsEOF, openFile, stdin, stdout
  , withFile
  )
import           System.IO.Error
  (ioeGetFileName, illegalOperationErrorType, isDoesNotExistError, isEOFError, IOErrorType, mkIOError)
import           System.Info (os)
import           System.Environment (lookupEnv)

import qualified Data.Text as T (Text, unpack)
import qualified Data.Text.IO as TIO (hGetLine, putStr, putStrLn)

import           Gadgets.Monad (void_)

type Text = T.Text

-- | Reads all lines from @stdin@, returning a @[String]@.
--
getLines :: IO [String]
getLines = hGetLines stdin

-- | Reads all lines from a @stdin@, returning a @[Text]@.
--
getLines' :: IO [Text]
getLines' = hGetLines' stdin

-- | Reads all lines from a handle, returning a @[String]@.
--
hGetLines :: Handle -> IO [String]
hGetLines hdl = do
  b <- hIsEOF hdl
  if b 
    then return []
    else liftM2 (:) (hGetLine hdl) (hGetLines hdl)

-- | Reads all lines from a handle, returning a @[Text]@.
--
hGetLines' :: Handle -> IO [Text]
hGetLines' hdl = do
  b <- hIsEOF hdl
  if b 
    then return []
    else liftM2 (:) (TIO.hGetLine hdl) (hGetLines' hdl)

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
handleDNEPath m 
  = handle $ \e -> if isDoesNotExistError e
    then m $ ioeGetFileName e
    else throw e

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

-- | Strictly outputs a @Text@ via @stdout@.
--      
putStr' :: Text -> IO ()
putStr' str 
  = TIO.putStr str >> hFlush stdout

-- | Making and throwing an @IOError@.
--
throwIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> a
throwIOError t s h p = throw $ mkIOError t s h p

-- | Making and throwing an @IOError@ without file path and handle.
--
throwIOError_ :: IOErrorType -> String -> a
throwIOError_ t s = throwIOError t s Nothing Nothing

-- | Similar to @withFile@ but takes a strict @Text@ as file path.
--
withFile' :: Text -> IOMode -> (Handle -> IO a) -> IO a
withFile' = withFile . T.unpack
