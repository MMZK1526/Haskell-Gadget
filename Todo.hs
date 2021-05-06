{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This is a simple to-do list management terminal line application, based on
-- the example in http://learnyouahaskell.com/input-and-output.
-- Compile the program and run it with argument "help" to see the documentation.

import           Control.Exception (IOException, SomeException, handle)
import           Control.Monad (forM_)

import           System.Directory (renameFile, removeFile)
import           System.Environment (getArgs)
import           System.IO (IOMode(..), hClose, openFile, openTempFile)
import           System.IO.Error (userErrorType)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Gadgets.IO (Text, hGetLines', throwIOError, throwIOError_)
import           Gadgets.Pure (ftext)

-- Table of commands
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("help", help)
            , ("remove", remove)
            , ("view", view)
            ]

helpMsg :: Text
helpMsg = 
  "\n    add: ./Todo add [filename.txt] [item 1] [item 2] ... [item n]\n\
  \Creates a file named [filename.txt] if it does not exist, and append the items as strings to the file.\n\
  \    view: ./Todo add [filename.txt]\n\
  \Numbers and lists out all items in the file of to-do list.\n\
  \    remove: ./Todo remove [filename.txt] [num 1] [num 2] ... [num n]\n\
  \Removes the items in the file corresponding to the given indices.\n\n\
  \    Example: \n\
  \$ ./Todo add foo.txt \"Manchester United\" \"Chelsea\" \"Nottingham Forest\" \"Aston Villa\" \"Liverpool\"\n\
  \$ ./Todo view foo.txt\n\
  \1 - Manchester United\n\
  \2 - Chelsea\n\
  \3 - Nottingham Forest\n\
  \4 - Aston Villa\n\
  \5 - Liverpool\n\
  \$ ./Todo remove foo.txt 3 4\n\
  \$ ./Todo view foo.txt\n\
  \1 - Manchester United\n\
  \2 - Chelsea\n\
  \3 - Liverpool\n"

-- Error Messages
noArgMsg :: String
noArgMsg = "Should have at least one argument"

wrongCmdMsg :: String
wrongCmdMsg = "This command does not exist"

noPathMsg :: String
noPathMsg = "Please provide a file path"

notIntMsg :: String
notIntMsg = "The arguments must be integers"

-- Main program
-- The second do block is run first. When an exception occurs, it is handled by
-- the first do block.
main :: IO ()
main = handle (\(e :: IOException) ->
  do print e
     putStrLn "Type \"todo help\" to see the help doc.") $
  do cargs <- getArgs
     if null cargs
       then throwIOError_ userErrorType noArgMsg
       else case lookup (ftext T.toLower $ head cargs) dispatch of
         Nothing -> throwIOError_ userErrorType wrongCmdMsg
         Just op -> op $ tail cargs

-- Documentation
help :: [String] -> IO ()
help = const $ TIO.putStrLn helpMsg                       

-- Adds a list of to-do items into the file
add :: [String] -> IO ()
add []             = throwIOError_ userErrorType noPathMsg
add (path : items) = do
  appendFile path ""
  forM_ items $ \i -> appendFile path (i ++ "\n")

-- Gets the content of the file
get :: String -> IO [Text]
get path = do
  hld <- openFile path ReadWriteMode
  con <- hGetLines' hld 
  hClose hld
  return con

-- Displays the content of the file
view :: [String] -> IO ()
view []         = throwIOError_ userErrorType noPathMsg
view (path : _) = do
  con <- get path
  if null con
    then putStrLn "The file is empty."
    else TIO.putStr $ T.unlines $
         zipWith (\i t -> T.concat [T.pack (show i), " - ", t]) [1..] con

-- Removes the items corresponding to the given indices
remove :: [String] -> IO ()
remove []            = throwIOError_ userErrorType noPathMsg
remove (path : nums) = do
  con <- get path
  (tempN, tempH) <- openTempFile "." ".114514.mmzk"
  handle (\(e :: SomeException) -> 
    do hClose tempH
       removeFile tempN
       throwIOError userErrorType notIntMsg Nothing (Just path)) $
    do let ns     = read <$> nums :: [Int]
       let newCon = fmap snd $ filter ((`notElem` ns) . fst) $ zip [1..] con
       TIO.hPutStr tempH $ T.unlines newCon
       hClose tempH
       removeFile path
       renameFile tempN path
