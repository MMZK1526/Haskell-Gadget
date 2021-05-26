{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception (finally)
import           Control.Monad (forever)
import           System.Environment (getArgs)
import           System.IO
  ( IOMode(..), SeekMode(..), hClose, hSeek, hSetFileSize, hTell, openFile
  , hPutChar
  )
import           System.IO.Error (userErrorType)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Gadgets.IO (handleEOF_, handleIO, throwIOError_)
import           Gadgets.Text (Text, replaceList)

helpMsg :: Text
helpMsg = "\n    ./BlockString [src.txt] [dest.txt]\n\
          \Reads the source file as a string and parse it into a multi-line Haskell String.\n\
          \One can copy the texts in the destination file straight into a Haskell source code as a String literal.\n\n\
          \    Example: \n\
          \> echo \"我卢本伟枪法招式灵活\n\
          \活到最后就是你的罪过\n\
          \锅的摆放构造取决于我\n\
          \我的子弹出膛无人能躲\" > in.txt\n\
          \> ./BlockString in.txt out.txt\n\
          \> cat out.txt\n\
          \\"我卢本伟枪法招式灵活\\n\\\n\
          \\\活到最后就是你的罪过\\n\\\n\
          \\\锅的摆放构造取决于我\\n\\\n\
          \\\我的子弹出膛无人能躲\"\n"

-- Error Messages
fewArgMsg :: String
fewArgMsg = "Should have at least two argument"

-- Decorators for multi-line strings
prefix, suffix :: Text
prefix = "\\"
suffix = "\\n\\\n"

main :: IO ()
main = handleIO (\e ->
  do print e
     putStrLn "Type \"blockstring help\" to see the help doc.") $
  do cargs <- getArgs
     case cargs of
       ["help"]       -> help
       src : dest : _ -> parseIO src dest
       _              -> throwIOError_ userErrorType fewArgMsg

-- Documentation
help :: IO ()
help = T.putStrLn helpMsg

-- Read a string from the source file, convert it, write it to the destination
parseIO :: FilePath -> FilePath -> IO ()
parseIO src dest = do
  hSrc  <- openFile src ReadMode
  hDest <- openFile dest WriteMode
  finally (
    -- try
    handleEOF_ $ do 
      ln <- T.hGetLine hSrc
      hPutChar hDest '\"'
      T.hPutStr hDest $ parseLine False ln
      handleEOF_ $ forever $ do
        ln <- T.hGetLine hSrc
        T.hPutStr hDest $ parseLine True ln
      size <- hTell hDest
      let offset = fromIntegral $ T.length suffix
      -- Remove the last suffix, changing it to a quotation mark
      hSeek hDest SeekFromEnd (-offset)
      hPutChar hDest '\"'
      hSetFileSize hDest (size - offset + 1)
    ) $ do 
    -- finally
      hClose hSrc
      hClose hDest

-- Add Haskell string literal line separator to the end and the front
parseLine :: Bool -> Text -> Text
parseLine True str
  = T.concat [prefix, escape str, suffix] 
parseLine _ str
  = T.concat [escape str, suffix] 

-- Deals with escape characters
escape :: Text -> Text
escape = replaceList [("\\", "\\\\"), ("\"", "\\\"")] 
