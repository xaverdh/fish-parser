module Fish.Parser.ReadP where

import qualified Fish.Parser.Parser as P
import Fish.Lang
import qualified Data.Text as T
import Text.ParserCombinators.ReadP

program :: ReadP (Prog T.Text ())
program = P.program
