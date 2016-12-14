module Fish.Parser.Trifecta where

import qualified Fish.Parser.Parser as P
import qualified Fish.Parser.Common as C
import Fish.Lang
import Text.Trifecta.Parser

program :: Parser (Prog ())
program = P.program

