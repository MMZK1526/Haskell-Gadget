module Gadgets.Text.Lazy where

import           Data.List (foldl')

import qualified Data.Text.Lazy as T

type Text = T.Text

-- | Take a list of pairs of needles and replacements, replace all occurrences
-- of the needles by the replacements.
-- Pre: The needles must be non-empty and disjoint.
-- 
replaceList :: [(Text, Text)] -> Text -> Text
replaceList = flip (foldl' go)
  where
    go text (orig, rep) = T.replace orig rep text
