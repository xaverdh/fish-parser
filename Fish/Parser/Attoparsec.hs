module Fish.Parser.Attoparsec where

import qualified Fish.Parser.Parser as P
import Fish.Lang
import qualified Data.Text as T
import Data.Attoparsec.Text

program :: Parser (Prog T.Text ())
program = P.program

