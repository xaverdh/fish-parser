module Fish.Parser.Trifecta where

import qualified Fish.Parser.Parser as P
import Fish.Lang.Lang
import Text.Trifecta.Parser

program :: Parser (Prog ())
program = P.program
