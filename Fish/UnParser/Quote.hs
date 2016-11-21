{-# language OverloadedStrings #-}
module Fish.UnParser.Quote (
  quoteSQ
  ,quoteDQ
  ,quoteNQ
) where

import qualified Data.Text as T
import Data.Monoid

quoteSQ :: T.Text -> T.Text
quoteSQ s =
  "'" <>
  q ['\'','\\'] s
  <> "'"

quoteDQ :: Int -> T.Text -> T.Text
quoteDQ n s =
  "\"" <>
  (wrap n . q ['\n','$','"','\\']) s
  <> "\""

quoteNQ :: Int -> T.Text -> T.Text
quoteNQ n = wrap n . q cs
  where
    cs = "\n\t\f\r\v\a\b $\\*?~%#(){}[]<>^&;,\"'|012."

q :: [Char] -> T.Text -> T.Text
q xs = T.concatMap quoteChar
  where
    quoteChar x
      | x `elem` xs = '\\' `T.cons` x `T.cons` T.empty
      | otherwise = x `T.cons` T.empty

wrap :: Int -> T.Text -> T.Text
wrap n =
  T.intercalate "\\\n"
  . parts
  where
    parts :: T.Text -> [T.Text]
    parts s = 
      let (s1,s2) = T.splitAt n s
       in case s2 of
        "" -> [s1]
        _ -> s1 : parts s2

