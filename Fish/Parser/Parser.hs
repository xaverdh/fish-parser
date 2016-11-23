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
import qualified Data.Char as C
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Lens hiding (Context,noneOf)

program :: PC m => m (Prog ())
program = (prog <* eof) `runReaderT` defaultContext

prog :: PC m => P m (Prog ())
prog = stmtSep *>
  ( Prog ()
    <$> (compStmt `sepEndBy` stmtSep1) )
  <?> "code-block"

progN :: PC m => P m (Prog ())
progN = stmtSep1 *>
  ( Prog ()
    <$> (compStmt `sepEndBy` stmtSep1) )
  <?> "code-block"

args :: PC m => P m (Args ())
args = Args ()
  <$> (expr `sepEndBy` spaces1)
  <?> "expressions"

compStmt :: PC m => P m (CompStmt ())
compStmt = do
  st <- stmt
  option (Simple () st)
    ( spaces *>
      ( piped st <|> forked st ) )
  where
    piped st = Piped ()
      <$> ( sym "|" $> StdOutFd
            <|> sym "1>|" $> StdOutFd 
            <|> sym "2>|" $> StdErrFd )
      <*> return st
      <*> compStmt
      <?> "pipe"

    forked st =
      sym "&"
      $> Forked () st
      <?> "fork-symbol"

stmt :: PC m => P m (Stmt ())
stmt = do
  st <- plain
  option st $ try (RedirectedSt () st <$> redirections)
  <?> "statement"
  where
    redirections = some redirect <?> "redirection"
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
  <$> lexemeN cmdIdent
  <*> args
  <?> "command-statement"

setSt :: PC m => P m (Stmt ())
setSt = symN "set" *>
  ( SetSt ()
    <$> optional
    ( (,) <$> lexemeN varDef <*> args ) )
  <?> "variable-definition"

funSt :: PC m => P m (Stmt ())
funSt = sym1 "function" *> (
    FunctionSt ()
    <$> lexemeN funIdent
    <*> args
    <*> progN
  ) <* symN "end"
  <?> "function-statement"

whileSt :: PC m => P m (Stmt ())
whileSt = sym1 "while" *>
  (WhileSt ()
    <$> stmt
    <*> progN <* symN "end")
  <?> "while-statement"

forSt :: PC m => P m (Stmt ())
forSt = sym1 "for" *>
  ( ForSt ()
    <$> (lexeme1 varIdent <* sym1 "in")
    <*> args
    <*> progN ) <* symN "end"
  <?> "for-statement"

ifSt :: PC m => P m (Stmt ())
ifSt =
  ( IfSt ()
    <$> ((:) <$> ifblock <*> many elif)
    <*> optional el ) <* symN "end"
  <?> "if-statement"
  where
    ifblock = sym1 "if" *>
      ((,) <$> stmt <*> progN)
    elif =
      lexeme1 (string "else" *> spaces1 *> string "if")
      *> ((,) <$> stmt <*> progN)
    el = symN "else" *> progN

switchSt :: PC m => P m (Stmt ())
switchSt = sym1 "switch" *>
  ( SwitchSt ()
    <$> lexemeN expr <*> (stmtSep1 *> many switchCase) )
  <* symN "end"
  <?> "switch-statement"
  where
    switchCase = sym1 "case" *>
      ( (,) <$> lexemeN expr <*> progN )

beginSt :: PC m => P m (Stmt ())
beginSt = symN "begin"
  *> ( BeginSt () <$> progN )
  <* symN "end"


andSt :: PC m => P m (Stmt ())
andSt = sym1 "and"
  *> (AndSt () <$> stmt)
  <?> "and-statement"

orSt :: PC m => P m (Stmt ())
orSt = sym1 "or"
  *> (OrSt () <$> stmt)
  <?> "or-statement"

notSt :: PC m => P m (Stmt ())
notSt = sym1 "not"
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

    intE = (StringE () . pack) <$> some digit <?> "integer"

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
    body = expr `sepBy` char ','

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
  <$> (char '%' *> lexemeN expr)
  <?> "process-expansion"

homeDirE :: PC m => P m (Expr ())
homeDirE = char '~' $> HomeDirE () <?> "~"

redirect :: PC m => P m (Redirect ())
redirect =
  choice [ to, from, err ] <?> "redirection"
  where
    outRedirect start app = start *>
      parseEither (char '&' *> fd)
      ( (,)
        <$> option False (app $> True)
        <*> (spaces *> lexemeN expr) )
    
    startErr = 
      ( string "2>"
        <* notFollowedBy (char '|') )
    
    err = Redirect (Right StdErrFd)
      <$> ( outRedirect (char '^') (char '^')
            <|> outRedirect startErr (char '>') )
      <?> "stderr-redirection"
    
    startTo = try
      ( skipOptional (char '1')
        *> char '>'
        <* notFollowedBy (char '|') )
    
    to = Redirect (Right StdOutFd)
      <$> outRedirect startTo (char '>')
      <?> "stdout-redirection"
    
    startFrom = try
      ( skipOptional (char '0')
        *> char '<' )
    
    from = startFrom *>
      ( Redirect (Left StdInFd) . Right . (False,)
        <$> ( spaces *> lexemeN expr) )
      <?> "stdin-redirection"

ref :: PC m => P m i -> P m (Ref i)
ref q = withContext array $
  optional (start *> body <* end)
  <?> "array-reference"
  where
    start = sym "[" <?> "index-range"
    end = char ']' <?> "end of index-range"
    body = range `sepEndBy` spaces1
    range = do
      i <- q
      option (Index i) ( Range i <$> (sym ".." *> q) )

cmdRef :: PC m => P m (CmdRef ())
cmdRef = withContext cmdSubst
  ( CmdRef ()
    <$> body
    <*> ref expr )
  where
    body = start *> prog <* end
    start = sym "(" <?> "command-substitution"
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
      <|> try ( oneOf' "012" <* notFollowedBy (oneOf' "><") )
    escPass =
      (escapeSequence <$> oneOf' "ntfrvabe")
      <|> try (char 'c' *> anyChar >>= controlSequence)
      <|> notChar '\n'
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
      'e' -> C.chr 0x1B
    
    controlSequence :: MonadPlus m => Char -> m Char
    controlSequence = \case
      '@'  -> return (C.chr 0)
      '['  -> return (C.chr 0x1B)
      '\\' -> return (C.chr 0x1C)
      ']'  -> return (C.chr 0x1D)
      '^'  -> return (C.chr 0x1E)
      '_'  -> return (C.chr 0x1F)
      '?'  -> return (C.chr 0x7F)
      c -> 
        let i = C.ord c - C.ord 'a'
         in if i >= 0 && i <= C.ord 'z'
            then return (C.chr i)
            else mzero

varIdent :: PC m => P m (VarIdent ())
varIdent = (VarIdent () . pack)
  <$> ((:) <$> letter <*> many (alphaNum <|> char '_'))
  <?> "variable-identifier"

funIdent :: PC m => P m (FunIdent ())
funIdent = (FunIdent () . pack)
  <$> some ( alphaNum <|> oneOf "_-" )
  <?> "function-identifier"

cmdIdent :: PC m => P m (CmdIdent ())
cmdIdent = (CmdIdent () . pack)
  <$> noTermString
        ( some $ alphaNum <|> oneOf "/_-" )
  <?> "command-identifier"
  where
    noTermString = mfilter $ not . (`elem` ["end","else"])

