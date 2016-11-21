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
  (tr . q ['\'','\\']) s
  <> "'"

quoteDQ :: Int -> T.Text -> T.Text
quoteDQ n s =
  "\"" <>
  (wrap n . q ['$','"','\\']) s
  <> "\""

quoteNQ :: Int -> T.Text -> T.Text
quoteNQ n = wrap n . tr . q cs
  where
    cs = " $\\*?~%#(){}[]<>^&;,\"'|012."

q :: [Char] -> T.Text -> T.Text
q xs = T.concatMap quoteChar
  where
    quoteChar x
      | x `elem` xs = '\\' `T.cons` x `T.cons` T.empty
      | otherwise = x `T.cons` T.empty

tr :: T.Text -> T.Text
tr = mconcat (T.map trChar)
  where
    trChar = \case
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\f' -> "\\f"
      '\r' -> "\\r"
      '\v' -> "\\v"
      '\a' -> "\\a"
      '\b' -> "\\b"
      c -> case C.ord c of
        0 -> "\\c@"
        0x1B -> "\\c["
        0x1C -> "\\c\\"
        0x1D -> "\\c]"
        0x1E -> "\\c^"
        0x1F -> "\\c_"
        0x7F -> "\\c?"
        i -> if i >= 0 && i <= C.ord 'z'
             then "\\c" <> (T.singleton . C.chr) (C.ord 'a' + i)
             else T.singleton c


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

