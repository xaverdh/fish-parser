module Fish.Parser.Fd where

import Fish.Parser.Common
import Fish.Lang.Lang

import Text.Parser.Combinators
import Text.Parser.Char hiding (space,spaces)
import Data.Functor
import Control.Applicative
import Control.Monad

fd :: PC m => P m Fd
fd = (Left <$> inFd)
  <|> (Right <$> outFd)
  <?> "file-descriptor"

inFd :: PC m => P m InFd
inFd = char '0' $> StdInFd

outFd :: PC m => P m OutFd
outFd = choice [
    char '1' $> StdOutFd
    ,char '2' $> StdErrFd
  ]
