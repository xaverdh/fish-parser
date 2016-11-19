{-# language OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving, ConstraintKinds, Rank2Types #-}
module Fish.Parser.Common where

import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.LookAhead
import Text.Parser.Char hiding (space,spaces)
import Data.Foldable (foldl')
import qualified Data.Char as C
import qualified Data.Text as T
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Lens hiding (Context)

type P m a = ReaderT Context m a
type PC m = 
  ( Functor m
    ,Applicative m
    ,Alternative m
    ,Monad m
    ,MonadPlus m
    ,Parsing m
    ,CharParsing m
    ,TokenParsing m
    ,LookAheadParsing m )

data Context = Context {
    _quoted :: Bool
    ,_array :: Bool
    ,_cmdSubst :: Bool
  }
makeLenses ''Context

defaultContext = Context False False False

withContext l = local (l .~ True)
resetContext l = local (l .~ False)

pack :: String -> T.Text
pack = T.pack

space :: PC m => P m Char
space =
  try (char '\\' *> char '\n')
  <|> ofUnicodeType C.Space
  <|> char '\t'
  <?> "space-char"

spaces :: PC m => P m ()
spaces = skipMany space <?> "space"

spaces1 :: PC m => P m ()
spaces1 = skipSome space <?> "space"

stmtSep :: PC m => P m ()
stmtSep = do
  c <- view cmdSubst
  if c
    then skipSome ( spaces *> void (char ';') <* spaces )
    else spaces *>
      ( skipSome (seps1 <* spaces)
        <|> (void . lookAhead) (char '#' <?> "comment") )
  <?> "statement seperator"
  where
    sep = oneOf "\n;"
    seps1 = skipSome sep
    

engulfedIn :: PC m => P m a -> P m b -> P m [a]
engulfedIn p f = skipOptional f *> (p `sepEndBy` f)

ofUnicodeType :: PC m => C.GeneralCategory -> P m Char
ofUnicodeType cat = satisfy $ (==cat) . C.generalCategory

notOfUnicodeType :: PC m => C.GeneralCategory -> P m Char
notOfUnicodeType cat = satisfy $ (/=cat) . C.generalCategory

parseEither :: PC m => P m a -> P m b -> P m (Either a b)
parseEither p1 p2 = (Left <$> p1) <|> (Right <$> p2)

