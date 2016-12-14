{-# LANGUAGE DeriveFunctor #-}
module Fish.Lang where

import qualified Data.Text as T
import qualified Data.List.NonEmpty as N
import Data.Bifunctor

-- | The type of string data, currently 'T.Text'.
type S = T.Text

-- | A fish program, consisting of several (composite) statements.
data Prog t = Prog t [CompStmt t]
  deriving (Eq,Ord,Show,Functor)

-- | A list of arguments, belonging to a command.
data Args t = Args t [Expr t]
  deriving (Eq,Ord,Show,Functor)

-- | A composite statement.
data CompStmt t =
  Simple t (Stmt t)
  -- ^ Wraps a simple statement
  | Piped t Fd (Stmt t) (CompStmt t)
  -- ^ A pipe connecting a simple statement and a composite statement
  | Forked t (Stmt t)
  -- ^ A forked statement
  deriving (Eq,Ord,Show,Functor)

data Stmt t = 
  CommentSt t S
  -- ^ A /comment/
  | CmdSt t (CmdIdent t) (Args t)
  -- ^ A /shell command/, has an identifier and arguments
  | SetSt t (SetCommand t)
  -- ^ The /set/ builtin command
  | FunctionSt t (FunIdent t) (Args t) (Prog t)
  -- ^ The /function/ builtin command
  | WhileSt t (Stmt t) (Prog t)
  -- ^ /while/ loops
  | ForSt t (VarIdent t) (Args t) (Prog t)
  -- ^ /for/ loops
  | IfSt t (N.NonEmpty (Stmt t,Prog t)) (Maybe (Prog t))
  -- ^ /if/ statements
  | SwitchSt t (Expr t) (N.NonEmpty (Expr t,Prog t))
  -- ^ /switch/ statements
  | BeginSt t (Prog t)
  -- ^ /begin/ statements
  | AndSt t (Stmt t)
  -- ^ /and/ statement modifier
  | OrSt t (Stmt t)
  -- ^ /or/ statement modifier
  | NotSt t (Stmt t)
  -- ^ /not/ statement modifier
  | RedirectedSt t (Stmt t) (N.NonEmpty (Redirect t))
  -- ^ A 'Stmt', annotated with redirections
  deriving (Eq,Ord,Show,Functor)

data Expr t =
  StringE t S
  -- ^ String expressions, can be \"..\"-type, \'..\'-type or plain strings.
  | GlobE t Glob
  -- ^ Glob patterns.
  | ProcE t (Expr t)
  -- ^ Process expansion, i.e. %last .
  | HomeDirE t
  -- ^ Home directory expansion, i.e. ~ .
  | VarRefE t Bool (VarRef t)
  -- ^ Variable references, i.e. $a. The boolean
  -- keeps track of whether the variable occured in \"\"-quotes.
  | BracesE t [Expr t]
  -- ^ Braces expansion, i.e. {..}.
  | CmdSubstE t (CmdRef t)
  -- ^ Command substitution, i.e. (..).
  | ConcatE t (Expr t) (Expr t)
  -- ^ One expression following the other without seperating whitespace.
  deriving (Eq,Ord,Show,Functor)

data SetCommand t = 
  SetSetting (Maybe Scope) (Maybe Export) (VarDef t) (Args t)
  -- ^ The /set/ builtin command in setting mode
  | SetList (Maybe Scope) (Maybe Export) Bool
  -- ^ The /set/ builtin command in list mode,
  --   boolean corresponds to the "-n" flag.
  | SetQuery (Maybe Scope) (Maybe Export) (Args t)
  -- ^ The /set/ builtin command in query mode
  | SetErase (Maybe Scope) (Args t)
  -- ^ The /set/ builtin command in erase mode
  deriving (Eq,Ord,Show,Functor)

-- | Export flag.
data Export = Export | UnExport
  deriving (Eq,Ord,Show,Bounded,Enum)

-- | A variable scope.
data Scope = 
  ScopeLocal
  | ScopeGlobal
  | ScopeUniversal
  deriving (Eq,Ord,Show,Bounded,Enum)

-- | Glob pattern, can be one of * ** ?
data Glob = 
  StarGl
  | DiStarGl
  | QMarkGl
  deriving (Eq,Ord,Show,Bounded,Enum)

-- | Variable identifiers
data VarIdent t = VarIdent t S
  deriving (Eq,Ord,Show,Functor)

-- | Function identifiers
data FunIdent t = FunIdent t S
  deriving (Eq,Ord,Show,Functor)

-- | Command name identifiers
data CmdIdent t = CmdIdent t S
  deriving (Eq,Ord,Show,Functor)

-- | A unix file descriptor from 0 to 9
data Fd =
    Fd0 | Fd1 | Fd2 | Fd3 | Fd4
  | Fd5 | Fd6 | Fd7 | Fd8 | Fd9
  deriving (Eq,Ord,Show,Bounded,Enum)

-- | Type of a redirection, the first file descriptor
--   is the fd being redirected, the second part is
--   the target.
--
--   It can be either another fd or a file,
--   in which case the boolean tells us whether it should
--   be overwritten (False) or appended to (True).
data Redirect t = 
  RedirectClose Fd
  | RedirectIn Fd ( Either Fd (Expr t) )
  | RedirectOut Fd ( Either Fd (FileMode,Expr t) )
  deriving (Eq,Ord,Show,Functor)

-- | Modes for writing to a file:
--
--   * 'FModeWrite'  : overwrite existing file
--   * 'FModeApp'    : append to existing file
--   * 'FModeNoClob' : refuse to write to existing file
data FileMode = FModeWrite | FModeApp | FModeNoClob
  deriving (Eq,Ord,Show,Bounded,Enum)
  

-- | An Index from an [..] index expression.
--   Can be either a single index or an index range.
data Indexing i = Index i | Range i i
  deriving (Eq,Ord,Show,Functor)

-- | An optional index expression, consisting
--   of a list of indices / index ranges.
type Ref i = Maybe [Indexing i]

-- | A variable reference starting with a name,
--   which may be
--
--   * another variable reference
--   * a variable identifier
--
--   potentially followed by an index expression.
data VarRef t = VarRef t
  ( Either (VarRef t) (VarIdent t) )
  ( Ref (Expr t) )
  deriving (Eq,Ord,Show)

instance Functor VarRef where
  fmap f (VarRef t a b) = 
    VarRef (f t)
    ( bimap (fmap f) (fmap f) a )
    ( fmap (map (fmap $ fmap f)) b )

-- | A variable definition expression, belonging to
--   the set builtin.
--
--   The only difference from 'VarRef'
--   is that the name must be a statically known identifier.
data VarDef t = VarDef t
  ( VarIdent t )
  ( Ref (Expr t) )
  deriving (Eq,Ord,Show,Functor)

-- | A command reference, given by a piece of fish code and
--   an optional index expression.
data CmdRef t = CmdRef t (Prog t) (Ref (Expr t))
  deriving (Eq,Ord,Show,Functor)


