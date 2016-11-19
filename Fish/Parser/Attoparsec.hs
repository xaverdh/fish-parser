module Fish.Parser.Attoparsec where

import qualified Fish.Parser.Parser as P
import Fish.Lang.Lang
import Data.Attoparsec.Text

program :: Parser (Prog ())
program = P.program
