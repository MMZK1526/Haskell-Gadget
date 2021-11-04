{-# LANGUAGE FlexibleContexts #-}
module Gadgets.CSV (CSV) where

import           Control.Monad (liftM2)
import           Data.Foldable (Foldable(..))
import           Data.List (intersperse)
import qualified Data.Sequence as A
import           Data.Sequence ((<|))
import qualified Text.Parsec as P
import           Text.Parsec ((<|>), (<?>))

-- | A CSV element.
data Element 
  = Integer Integer 
  | Double Double
  | String String
  | Null
  deriving Eq

instance Show Element where
  show (Integer i) = show i
  show (Double  d) = show d
  show (String  s) = show s
  show Null        = ""

-- | The CSV structure.
data CSV = CSV 
  { delimiter :: Char
  , content :: A.Seq (A.Seq Element)
  }
  deriving (Eq, Show)

-- instance Show CSV where
--   show (CSV c csv) 
--     = concat $ toList $ A.intersperse "\n" $ intersperse c . show . toList <$> csv

-- | Parse a String into a Comma Separated Value.
toCSV :: String -> Either P.ParseError CSV
toCSV = toCSVWithDelimiter ','

-- | Parse a String into a Colon Separated Value.
toCSV' :: String -> Either P.ParseError CSV
toCSV' = toCSVWithDelimiter ';'

-- | Parse a String into a TSV.
toTSV :: String -> Either P.ParseError CSV
toTSV = toCSVWithDelimiter '\t'

-- | Take a delimiter and a String, parse it into a CSV.
toCSVWithDelimiter :: Char -> String -> Either P.ParseError CSV
toCSVWithDelimiter c
  = P.parse (CSV c <$> parser') "CSV Parser: "
  where
    parser'     = (P.eof >> return A.empty) <|> do
      line <- lineParser
      P.try (P.char '\n' >> fmap (line <|) parser') 
        <|> return (A.singleton line)
    validChar   = P.satisfy (`notElem` (c : "\n"))
    intParser   = Integer . read <$> do
      i <- liftM2 (:) (P.oneOf "-0123456789") (P.many P.digit)
      b <- P.try (validChar >> return False) <|> return True
      if b then return i else fail "integer"
    floatParser = Double . read <$> do
      let properFloatParser b = do
          i <-  P.oneOf $ (if b then "" else ".") ++ "0123456789"
          r <-  properFloatParser (i == '.') 
            <|> return (if i == '.' then "0" else "")
          return $ i : r
      neg <- P.try (P.char '-' >> return True) <|> return False
      pt  <- P.try (P.char '.' >> return True) <|> return False
      f   <- properFloatParser pt
      b   <- P.try (validChar >> return False) <|> return True
      if b 
        then return $ if neg then '-' : f else f 
        else fail "double"
    strParser   = 
      let strParser' = P.try (liftM2 (:) validChar strParser') <|> return ""
      in String <$> strParser'
    lineParser  = (P.char '\n' >> return A.empty) <|>
      A.fromList <$> P.sepBy 
        (P.try intParser <|> P.try floatParser <|> strParser) (P.char c)
