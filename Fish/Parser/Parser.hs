{-# language LambdaCase, TupleSections, OverloadedStrings #-}
module Fish.Parser.Parser where

import Fish.Parser.Common
import Fish.Parser.Gen
import Fish.Parser.Glob
import Fish.Parser.Fd
import Fish.Lang.Lang

import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Char hiding (space,spaces)
import Data.Functor
import Data.Bool
import Data.Monoid
import Data.CharSet
import qualified Data.Text as T
import Data.String (IsString(..))
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Lens hiding (Context,noneOf)

program :: PC m => m (Prog ())
program = (prog <* eof) `runReaderT` defaultContext

prog :: PC m => P m (Prog ())
prog = Prog ()
  <$> (compStmt `engulfedIn` stmtSep)
  <?> "code-block"

args :: PC m => P m (Args ())
args = Args ()
  <$> (expr `engulfedIn` spaces1)
  <?> "expressions"

compStmt :: PC m => P m (CompStmt ())
compStmt = do
  st <- stmt
  option (Simple () st) (try (piped st) <|> try (forked st))
  where
    piped st = Piped ()
      <$> (
          spaces *>
          option StdOutFd (outFd <* char '>')
          <* char '|'
          <* spaces
        )
      <*> return st
      <*> compStmt
      <?> "pipe"

    forked st =
      spaces
      *> char '&'
      *> spaces
      $> Forked () st
      <?> "fork-symbol"

stmt :: PC m => P m (Stmt ())
stmt = do
  st <- plain
  option st $ try (RedirectedSt () st <$> redirections)
  <?> "statement"
  where
    redirections = some (spaces *> redirect) <?> "redirection"
    plain = choice [
        commentSt
        ,setSt
        ,funSt
        ,whileSt
        ,forSt
        ,ifSt
        ,switchSt
        ,beginSt
        ,andSt
        ,orSt
        ,notSt
        ,cmdSt
      ]

commentSt :: PC m => P m (Stmt ())
commentSt = (CommentSt () . pack)
  <$> (char '#' *> many (notChar '\n'))
  <?> "comment-statement"

cmdSt :: PC m => P m (Stmt ())
cmdSt = CmdSt ()
  <$> cmdIdent
  <*> args
  <?> "command-statement"

setSt :: PC m => P m (Stmt ())
setSt = SetSt () <$> try
    ( string "set" *> optional
      ( spaces1 *>
        ( (,) <$> varDef <*> args ) ) )
  <?> "variable-definition"

funSt :: PC m => P m (Stmt ())
funSt = try (string "function" *> spaces1) *> (
    FunctionSt ()
    <$> funIdent
    <*> args
    <*> prog
  ) <* string "end"
  <?> "function-statement"

whileSt :: PC m => P m (Stmt ())
whileSt = try (string "while" *> spaces1) *>
  (WhileSt ()
    <$> stmt
    <*> prog <* string "end")
  <?> "while-statement"

forSt :: PC m => P m (Stmt ())
forSt = try (string "for" *> spaces1) *>
  ( ForSt ()
    <$> (varIdent <* spaces1 <* string "in")
    <*> (spaces1 *> args)
    <*> prog ) <* string "end"
  <?> "for-statement"

ifSt :: PC m => P m (Stmt ())
ifSt =
  ( IfSt ()
    <$> ((:) <$> ifblock <*> many elif)
    <*> optional el ) <* string "end"
  <?> "if-statement"
  where
    ifblock = try (string "if" *> spaces1) *>
      ((,) <$> stmt <*> prog)
    elif =
      try ( string "else" *> spaces1
      *> string "if" *> space )
      *> ((,) <$> stmt <*> prog)
    el = try (string "else") *> prog

switchSt :: PC m => P m (Stmt ())
switchSt = try (string "switch" *> spaces1)
  *> ( SwitchSt () <$> (expr <* stmtSep) <*> many switchCase )
  <* string "end"
  <?> "switch-statement"
  where
    switchCase = try (string "case" *> space) *> (
        (,) <$> (expr <* stmtSep) <*> prog
      )

beginSt :: PC m => P m (Stmt ())
beginSt = try (string "begin" *> spaces)
  *> ( BeginSt () <$> prog )
  <* string "end"


andSt :: PC m => P m (Stmt ())
andSt = try (string "and" *> spaces1)
  *> (AndSt () <$> stmt)
  <?> "and-statement"

orSt :: PC m => P m (Stmt ())
orSt = try (string "or" *> spaces1)
  *> (OrSt () <$> stmt)
  <?> "or-statement"

notSt :: PC m => P m (Stmt ())
notSt = try (string "not" *> spaces1)
  *> (NotSt () <$> stmt)
  <?> "not-statement"

expr :: PC m => P m (Expr ())
expr = do
  q <- view quoted
  s <- if q then exprSimpleQ else exprSimple
  option s (ConcatE () s <$> expr)
  <?> "expression"
  where
    exprSimpleQ = intE <|> varRefE
    
    exprSimple = strlike
      <|> resetContext array
        ( choice [
            varRefE
            ,cmdSubstE
            ,bracesE
            ,globE
            ,homeDirE
            ,procE ] )

    intE = (StringE () . T.pack) <$> some digit <?> "integer"

varRefE :: PC m => P m (Expr ())
varRefE = VarRefE ()
  <$> view quoted
  <*> varRef
  <?> "variable-reference"

bracesE :: PC m => P m (Expr ())
bracesE = BracesE () <$> (
    start *> body <* end
  ) <?> "braces-substitution"
  where
    start = char '{'
    end = char '}' <?> "end of braces-substitution"
    body = (expr `sepBy1` char ',') <|> (pure <$> expr)

cmdSubstE :: PC m => P m (Expr ())
cmdSubstE = CmdSubstE ()
  <$> cmdRef
  <?> "command-substitution"

globE :: PC m => P m (Expr ())
globE = GlobE ()
  <$> glob
  <?> "glob-pattern"

procE :: PC m => P m (Expr ())
procE = ProcE ()
  <$> (char '%' *> expr)
  <?> "process-expansion"

homeDirE :: PC m => P m (Expr ())
homeDirE = char '~' $> HomeDirE () <?> "~"

redirect :: PC m => P m (Redirect ())
redirect =
  choice [ to, from, err ] <?> "redirection"
  where
    appendBit :: PC m => Char -> P m Append
    appendBit c = option False (char c $> True)
    
    err = char '^' *> (
        Redirect (Right StdErrFd)
        <$> parseEither (char '&' *> fd) (
            (,)
            <$> appendBit '^'
            <*> (spaces *> expr <* spaces)
          )
      ) <?> "stderr-redirection"
    
    to =
      (char '>' <* notFollowedBy (char '|')) *> (
        Redirect (Right StdOutFd)
        <$> parseEither (char '&' *> fd) (
            (,)
            <$> appendBit '>'
            <*> (spaces *> expr <* spaces)
          )
      ) <?> "stdout-redirection"
    
    from =
      char '<' *> (
        Redirect (Left StdInFd)
        . Right . (False,) <$> (spaces *> expr <* spaces)
      ) <?> "stdin-redirection"

ref :: PC m => P m i -> P m (Ref i)
ref q = withContext array $
  optional (start *> range <* end)
  <?> "array-reference"
  where
    start = char '[' <?> "index-range"
    end = char ']' <?> "end of index-range"
    range = do
      spaces
      some $ do
        i <- q
        r <- option (Index i) (
            Range i <$> (string ".." *> q)
          )
        sep
        return r
    sep = void spaces1 <|> void (lookAhead (char ']'))

cmdRef :: PC m => P m (CmdRef ())
cmdRef = withContext cmdSubst
  ( CmdRef ()
    <$> body
    <*> ref expr )
  where
    body = start *> prog <* end
    start = char '(' <?> "command-substitution"
    end = char ')' <?> "end of command-substitution"

varRef :: PC m => P m (VarRef ())
varRef = VarRef ()
  <$> name
  <*> ref expr
  <?> "variable-reference"
  where
    name = char '$' *> 
      parseEither varRef varIdent

varDef :: PC m => P m (VarDef ())
varDef = VarDef ()
  <$> name
  <*> ref expr
  <?> "variable-definition"
  where
    name = varIdent


instance IsString CharSet where
  fromString = fromList

noneOf' :: PC m => CharSet -> P m Char
noneOf' = noneOfSet

oneOf' :: PC m => CharSet -> P m Char
oneOf' = oneOfSet

strlike :: PC m => P m (Expr ())
strlike = 
  strSQ
  <|> strDQ
  <|> strNQ
  <?> "string"

strSQ :: PC m => P m (Expr ())
strSQ = (StringE () . pack) <$> (
    start *>
    strGen allowed escPass escSwallow escIgnore allowEmpty
    <* end
  )
  where
    start = char '\''
    end = char '\'' <?> "end of string"
    allowed = noneOf' "'\\"
    escPass = oneOf' "'\\"
    escSwallow = mzero
    escIgnore = noneOf' "'\\"
    allowEmpty = True


strDQ :: PC m => P m (Expr ())
strDQ = withContext quoted
  ( start *>
    option
      ( StringE () "" )
      ( foldl1 (ConcatE ()) <$> some (qstrE <|> expr) )
    <* end )
  
  where
    start = char '"'
    end = char '"' <?> "end of string"

    qstrE = (StringE () . pack) <$>
      strGen allowed escPass escSwallow escIgnore allowEmpty

    allowed = noneOf' "\"\\$\n"
    escPass = oneOf' "\"\\$"
    escSwallow = char '\n'
    escIgnore = noneOf' "\n\"\\$"
    allowEmpty = False

strNQ :: PC m => P m (Expr ())
strNQ = do
  ar <- view array
  (StringE () . pack)
    <$> strGen (allowed ar) escPass escSwallow escIgnore allowEmpty
  where
    invalid ar = 
      "\n\t $\\*?~%#(){}[]<>^&;,\"\'|012" <> bool "" "." ar
    allowed ar =
      noneOf' (invalid ar)
      <|> try ( oneOf' "012" <* notFollowedBy (char '>') )
    escPass =
      (escapeSequence <$> oneOf' "ntfrvab")
      <|> noneOf' "ec\n"
    escSwallow = char '\n'
    escIgnore = mzero
    allowEmpty = False

    escapeSequence :: Char -> Char
    escapeSequence = \case
      'n' -> '\n'
      't' -> '\t'
      'f' -> '\f'
      'r' -> '\r'
      'v' -> '\v'
      'a' -> '\a'
      'b' -> '\b'

varIdent :: PC m => P m (VarIdent ())
varIdent = (VarIdent () . pack)
  <$> try ( (:) <$> letter <*> many alphaNum )
  <?> "variable-identifier"

funIdent :: PC m => P m (FunIdent ())
funIdent = (FunIdent () . pack)
  <$> try ( some $ alphaNum <|> oneOf "_-" )
  <?> "function-identifier"

cmdIdent :: PC m => P m (CmdIdent ())
cmdIdent = (CmdIdent () . pack)
  <$> try ( noTermString
            ( some $ alphaNum <|> oneOf "/_-" ) )
  <?> "command-identifier"
  where
    noTermString = mfilter $ not . (`elem` ["end","else"])


