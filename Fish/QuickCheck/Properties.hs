{-# language OverloadedStrings #-}
module Fish.QuickCheck.Properties where

import Fish.Lang

import Fish.Parser.Parser
import Fish.Parser.Common
import Fish.Parser.Glob

import Fish.UnParser.UnParser

import Fish.QuickCheck.Arbitrary

import Data.Semigroup
import qualified Data.Text as T
import Data.Attoparsec.Text
import Test.QuickCheck hiding (Args)

attoParse p = parseOnly (p <* endOfInput)

parseUnparse p x = 
  attoParse (runp p) (unparse x)
  == Right x

parseUnparse' p x = 
  attoParse (runp p) (unparse x <> " ")
  == Right x

checkParseUnparse = do
  -- quickCheck
  -- quickCheck
  -- quickCheck
  quickCheck prop_parse_unparse_glob
  quickCheck prop_parse_unparse_varIdent
  quickCheck prop_parse_unparse_funIdent
  quickCheck prop_parse_unparse_cmdIdent

prop_parse_unparse_glob :: Glob -> Bool
prop_parse_unparse_glob =
  parseUnparse glob

prop_parse_unparse_varIdent :: VarIdent T.Text () -> Bool
prop_parse_unparse_varIdent = 
  parseUnparse varIdent

prop_parse_unparse_funIdent :: FunIdent T.Text () -> Bool
prop_parse_unparse_funIdent = 
  parseUnparse funIdent

prop_parse_unparse_cmdIdent :: CmdIdent T.Text () -> Bool
prop_parse_unparse_cmdIdent =
  parseUnparse cmdIdent


