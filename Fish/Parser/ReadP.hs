module Fish.Parser.ReadP where

import qualified Fish.Parser.Parser as P
import Fish.Lang
import Text.ParserCombinators.ReadP

program :: ReadP (Prog ())
program = P.program
