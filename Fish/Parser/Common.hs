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
import Data.String (IsString(..))
import Data.CharSet
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Lens hiding (Context)

-- | A Monad Transformer which gives access to the parser 'Context'.
type P = ReaderT Context

-- | The parsing \"Monad\". We use /ConstraintKinds/ instead of
--   a concrete Monad 
--
--   to keep the code polymorphic over the 
--   parsing framework ("Parsec","Trifecta" etc.).
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

-- | The parser 'Context':
--
--   * '_quoted' is toggled by entering a double quote type string,
--   * '_array' is toggled by entering an array index expression [..]
data Context = Context {
    _quoted :: Bool
    ,_array :: Bool
  }
makeLenses ''Context

-- | The starting 'Context'.
defaultContext = Context False False

-- | Run a parser in a given 'Context'.
runpInContext :: PC m => Context -> P m a -> m a
runpInContext ctxt p = (p <* eof) `runReaderT` ctxt

-- | Run a parser in the 'defaultContext'.
runp :: PC m => P m a -> m a
runp p = (p <* eof) `runReaderT` defaultContext

-- | Toggle a 'Context' switch ON.
withContext l = local (l .~ True)

-- | Toggle a 'Context' switch OFF.
resetContext l = local (l .~ False)

-- | We define our own pack function, so the underlying type
--   for string data is easily changed.
pack :: String -> T.Text
pack = T.pack

instance IsString CharSet where
  fromString = fromList

-- | An alias, such that the underlying implementation
--   can be switched between 'CharSet' and plain 'noneOf'.
noneOf' :: PC m => CharSet -> P m Char
noneOf' = noneOfSet

-- | An alias, such that the underlying implementation
--   can be switched between 'CharSet' and plain 'oneOf'.
oneOf' :: PC m => CharSet -> P m Char
oneOf' = oneOfSet

-- | The space parser, accepts all space (as in 'Data.Char.isSpace'),
--   except for the newline character.
space :: PC m => P m Char
space = 
  try (char '\\' *> char '\n')
  <|> satisfy ((&&) <$> C.isSpace <*> (/= '\n'))
  <?> "space-char"

-- | Skip any number of 'space' occurances.
spaces :: PC m => P m ()
spaces = skipMany space <?> "space"

-- | Like 'spaces', but fail unless
--   at least one space character was consumed.
spaces1 :: PC m => P m ()
spaces1 = skipSome space <?> "space"

-- | Turn a parser into a lexeme unit, which will not consume any
--   input on failure 
--
--   and swallow trailing white space on success.
lexeme :: PC m => P m a -> P m a
lexeme p = try p <* spaces

-- | Same as 'lexeme' but only succeed if at least one
--   trailing white space was consumed.
lexeme1 :: PC m => P m a -> P m a
lexeme1 p = try (p <* spaces1)

-- | Succeeds if the next character is of a special class,
--   considered /terminating/ characters.
--
--   Currently these are \\n ;  ) | > \^ \< #
terminated :: PC m => P m ()
terminated = void . lookAhead $ oneOf' "\n;)|>^<#"

-- | Same as 'lexeme' but only succeed if either:
--
--   * at least one trailing white space was consumed
--   * the next character is successfully parsed by 'terminated'.
lexemeN :: PC m => P m a -> P m a
lexemeN p = try (p <* (spaces1 <|> terminated))

-- | Turn a string into a 'lexeme' parser, parsing that string
--   and returning ()
sym :: PC m => String -> P m ()
sym = lexeme . void . string

-- | Turn a string into a 'lexeme1' parser, parsing that string
--   and returning ()
sym1 :: PC m => String -> P m ()
sym1 = lexeme1 . void . string

-- | Turn a string into a 'lexemeN' parser, parsing that string
--   and returning ()
symN :: PC m => String -> P m ()
symN = lexemeN . void . string

-- | Skip over a number of statement seperators.
stmtSep :: PC m => P m ()
stmtSep =
  skipMany (seps1 <* spaces)
  <?> "statement seperator"
  where
    sep = satisfy $ (||) <$> (== '\n') <*> (==';')
    seps1 = skipSome sep

-- | Like 'stmtSep', but make sure at least one
--   newline / semicolon was consumed.
stmtSep1 :: PC m => P m ()
stmtSep1 =
  skipSome (seps1 <* spaces)
  <|> (void . lookAhead) (char '#' <?> "comment")
  <?> "statement seperator"
  where
    sep = satisfy $ (||) <$> (== '\n') <*> (==';')
    seps1 = skipSome sep

-- | Parser one of two alternatives, return an Either.
parseEither :: PC m => P m a -> P m b -> P m (Either a b)
parseEither p1 p2 = (Left <$> p1) <|> (Right <$> p2)

