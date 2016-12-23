{-# language FlexibleContexts #-}
module Fish.Parser.Parsec where

import qualified Fish.Parser.Parser as P
import Fish.Lang
import qualified Data.Text as T
import Text.Parsec
import Data.Functor.Identity

program :: Stream s Identity Char => Parsec s () (Prog T.Text ())
program = P.program
