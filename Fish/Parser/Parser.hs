{-# language LambdaCase, TupleSections, OverloadedStrings, FlexibleContexts #-}
module Fish.Parser.Parser where

import qualified Fish.Parser.Redirect as Redirect
import Fish.Parser.Common
import Fish.Parser.Gen
import Fish.Parser.Glob
import Fish.Lang

import Text.Parser.Permutation
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Char hiding (space,spaces)
import Data.Functor
import Data.Bool
import Data.Semigroup hiding (option)
import Data.NText
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.List.NonEmpty as N
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Lens hiding (Context,noneOf)

program :: PC m => m (Prog T.Text ())
program = runp prog

prog :: PC m => P m (Prog T.Text ())
prog = stmtSep *>
  ( Prog ()
    <$> (compStmt `sepEndBy` stmtSep1) )
  <?> "code-block"

progN :: PC m => P m (Prog T.Text ())
progN = stmtSep1 *>
  ( Prog ()
    <$> (compStmt `sepEndBy` stmtSep1) )
  <?> "code-block"

args :: PC m => P m (Args T.Text ())
args = Args ()
  <$> (expr `sepEndBy` spaces1)
  <?> "expressions"

compStmt :: PC m => P m (CompStmt T.Text ())
compStmt = do
  st <- stmt
  option (Simple () st)
    ( spaces *>
      ( piped st <|> forked st ) )
  where
    piped st = Piped ()
      <$> ( sym "|" $> Fd1
            <|> sym "1>|" $> Fd1
            <|> sym "2>|" $> Fd2 )
      <*> return st
      <*> compStmt
      <?> "pipe"

    forked st =
      sym "&"
      $> Forked () st
      <?> "fork-symbol"

stmt :: PC m => P m (Stmt T.Text ())
stmt = do
  st <- plain
  option st $ try (RedirectedSt () st <$> redirects)
  <?> "statement"
  where
    redirects = 
      N.fromList
      <$> some redirect
      <?> "redirections"
    
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

commentSt :: PC m => P m (Stmt T.Text ())
commentSt = (CommentSt () . T.pack)
  <$> (char '#' *> many (notChar '\n'))
  <?> "comment-statement"

cmdSt :: PC m => P m (Stmt T.Text ())
cmdSt = CmdSt ()
  <$> lexemeN cmdIdent
  <*> args
  <?> "command-statement"

setSt :: PC m => P m (Stmt T.Text ())
setSt = symN "set" *>
  ( SetSt () <$> setCommand )
  <?> "set-statement"

data SetMode = Erase | Query | Setting

setCommand :: PC m => P m (SetCommand T.Text ())
setCommand = try setSQE <|> try setHelp <|> setList
  where
    setHelp = SetHelp <$ ( symN "--help" <|> symN "-h" )
    
    setList = permute ( SetList
      <$?> (Nothing,Just <$> scope)
      <|?> (Nothing,Just <$> export)
      <|?> (False,flag True "n" "names") )
      `evalStateT` False
    setSQE = do
      (fmode,mscp,fexport) <- permute
        ( (,,)
          <$?> (Setting,mode)
          <|?> (Nothing,Just <$> scope)
          <|?> (Nothing,Just <$> export) )
        `evalStateT` False
      case fmode of
        Setting ->
          SetSetting mscp fexport
          <$> lexemeN varDef
          <*> args
        Erase ->
          SetErase mscp . N.fromList <$> some (lexemeN varDef)
        Query ->
          SetQuery mscp fexport <$> args

    scope = choice
      [ flag ScopeLocal "l" "local"
       ,flag ScopeGlobal "g" "global"
       ,flag ScopeUniversal "U" "universal" ]

    export = choice
      [ flag Export "x" "export"
       ,flag UnExport "u" "unexport" ]

    mode = choice
      [ flag Erase "e" "erase"
       ,flag Query "q" "query" ]
    
    symN' s = lift $ symN s
    
    flag value short long =
      get >>= \case
        True -> value <$
          ( symN' short <* put False
            <|> (void . string) short )
        False -> value <$
          ( symN' ("-" <> short)
            <|> (try . void . string) ("-" <> short) <* put True
            <|> symN' ("--" <> long) )

funSt :: PC m => P m (Stmt T.Text ())
funSt = sym1 "function" *> (
    FunctionSt ()
    <$> lexemeN funIdent
    <*> args
    <*> progN
  ) <* symN "end"
  <?> "function-statement"

whileSt :: PC m => P m (Stmt T.Text ())
whileSt = sym1 "while" *>
  (WhileSt ()
    <$> stmt
    <*> progN <* symN "end")
  <?> "while-statement"

forSt :: PC m => P m (Stmt T.Text ())
forSt = sym1 "for" *>
  ( ForSt ()
    <$> (lexeme1 varIdent <* sym1 "in")
    <*> args
    <*> progN ) <* symN "end"
  <?> "for-statement"

ifSt :: PC m => P m (Stmt T.Text ())
ifSt =
  ( IfSt () . N.fromList
    <$> ( (:) <$> ifblock <*> many elif )
    <*> optional el ) <* symN "end"
  <?> "if-statement"
  where
    ifblock = sym1 "if" *>
      ((,) <$> stmt <*> progN)
    elif =
      lexeme1 (string "else" *> spaces1 *> string "if")
      *> ((,) <$> stmt <*> progN)
    el = symN "else" *> progN

switchSt :: PC m => P m (Stmt T.Text ())
switchSt = sym1 "switch" *>
  ( SwitchSt ()
    <$> lexemeN expr
    <*> ( stmtSep1 *>
          ( N.fromList <$> some switchCase ) ) )
  <* symN "end"
  <?> "switch-statement"
  where
    switchCase = sym1 "case" *>
      ( (,) <$> lexemeN expr <*> progN )

beginSt :: PC m => P m (Stmt T.Text ())
beginSt = symN "begin"
  *> ( BeginSt () <$> progN )
  <* symN "end"


andSt :: PC m => P m (Stmt T.Text ())
andSt = sym1 "and"
  *> (AndSt () <$> stmt)
  <?> "and-statement"

orSt :: PC m => P m (Stmt T.Text ())
orSt = sym1 "or"
  *> (OrSt () <$> stmt)
  <?> "or-statement"

notSt :: PC m => P m (Stmt T.Text ())
notSt = sym1 "not"
  *> (NotSt () <$> stmt)
  <?> "not-statement"

expr :: PC m => P m (Expr T.Text ())
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

    intE = (StringE () . pack)
      <$> some (digit <|> char '-' <|> char '+')
      <?> "integer"

varRefE :: PC m => P m (Expr T.Text ())
varRefE = VarRefE ()
  <$> view quoted
  <*> varRef
  <?> "variable-reference"

bracesE :: PC m => P m (Expr T.Text ())
bracesE = BracesE () <$> (
    start *> body <* end
  ) <?> "braces-substitution"
  where
    start = char '{'
    end = char '}' <?> "end of braces-substitution"
    body = expr `sepBy` char ','

cmdSubstE :: PC m => P m (Expr T.Text ())
cmdSubstE = CmdSubstE ()
  <$> cmdRef
  <?> "command-substitution"

globE :: PC m => P m (Expr T.Text ())
globE = GlobE ()
  <$> glob
  <?> "glob-pattern"

procE :: PC m => P m (Expr T.Text ())
procE = ProcE ()
  <$> (char '%' *> expr)
  <?> "process-expansion"

homeDirE :: PC m => P m (Expr T.Text ())
homeDirE = char '~' $> HomeDirE () <?> "~"

redirect :: PC m => P m (Redirect (Expr T.Text ()))
redirect = Redirect.redirect (lexemeN expr)

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

cmdRef :: PC m => P m (CmdRef T.Text ())
cmdRef = withContext cmdSubst
  ( CmdRef ()
    <$> body
    <*> ref expr )
  where
    body = start *> prog <* end
    start = sym "(" <?> "command-substitution"
    end = char ')' <?> "end of command-substitution"

varRef :: PC m => P m (VarRef T.Text ())
varRef = VarRef ()
  <$> name
  <*> ref expr
  <?> "variable-reference"
  where
    name = char '$' *> 
      parseEither varRef varIdent

varDef :: PC m => P m (VarDef T.Text ())
varDef = VarDef ()
  <$> name
  <*> ref expr
  <?> "variable-definition"
  where
    name = varIdent

strlike :: PC m => P m (Expr T.Text ())
strlike = 
  strSQ
  <|> strDQ
  <|> strNQ
  <?> "string"

strSQ :: PC m => P m (Expr T.Text ())
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


strDQ :: PC m => P m (Expr T.Text ())
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

strNQ :: PC m => P m (Expr T.Text ())
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

varIdent :: PC m => P m (VarIdent T.Text ())
varIdent = (VarIdent () . mkNText . T.pack)
  <$> ((:) <$> letter <*> many (alphaNum <|> char '_'))
  <?> "variable-identifier"

funIdent :: PC m => P m (FunIdent T.Text ())
funIdent = (FunIdent () . mkNText . T.pack)
  <$> some ( alphaNum <|> oneOf "_-" )
  <?> "function-identifier"

cmdIdent :: PC m => P m (CmdIdent T.Text ())
cmdIdent = (CmdIdent () . mkNText . T.pack)
  <$> noTermString
        ( some $ alphaNum <|> oneOf "/_-" )
  <?> "command-identifier"
  where
    noTermString = mfilter $ not . (`elem` ["end","else","case"])

