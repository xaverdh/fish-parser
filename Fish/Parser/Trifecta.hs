module Fish.Parser.Trifecta where

import qualified Fish.Parser.Parser as P
import qualified Fish.Parser.Common as C
import Fish.Lang
import qualified Data.Text as T
import Text.Trifecta.Parser

program :: Parser (Prog T.Text ())
program = P.program

