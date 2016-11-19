{-# language RankNTypes #-}
module Fish.Parser.Gen where

import Fish.Parser.Common

import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Char hiding (space,spaces)
import Data.Maybe (catMaybes)
import Control.Applicative

strGen :: PC m 
  => P m Char
  -> P m Char
  -> P m Char
  -> P m Char
  -> Bool
  -> P m String

strGen allowed escPass escSwallow escIgnore allowEmpty =
  catMaybes <$> (if allowEmpty then many else some) charGen
  where
    charGen = (Just <$> allowed) <|> try escaped
    escaped = char '\\' *> ( ignore <|> pass <|> swallow )
    pass = Just <$> escPass
    swallow = escSwallow *> return Nothing
    ignore = Just <$> ( lookAhead escIgnore *> return '\\' )




