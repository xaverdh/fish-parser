{-# LANGUAGE DeriveFunctor #-}
module Fish.Lang.Lang where

import qualified Data.Text as T
import Data.Bifunctor

-- | The type of string data, currently 'T.Text'.
type S = T.Text

-- | A program, consisting of several (composite) statements.
data Prog t = Prog t [CompStmt t]
  deriving (Eq,Ord,Show,Functor)

-- | A list of arguments, belonging to a command.
data Args t = Args t [Expr t]
  deriving (Eq,Ord,Show,Functor)

-- | A composite statement.
data CompStmt t =
  Simple t (Stmt t)
  -- ^ Wraps a simple statement
  | Piped t OutFd (Stmt t) (CompStmt t)
  -- ^ A pipe connecting a simple statement and a composite statement
  | Forked t (Stmt t)
  -- ^ A forked statement
  deriving (Eq,Ord,Show,Functor)

data Stmt t = 
  CommentSt t S
  -- ^ A comment
  | CmdSt t (CmdIdent t) (Args t)
  -- ^ A comment
  | SetSt t (Maybe ((VarDef t),(Args t)))
  -- ^ The /set/ builtin command
  | FunctionSt t (FunIdent t) (Args t) (Prog t)
  -- ^ The /function/ builtin command
  | WhileSt t (Stmt t) (Prog t)
  -- ^ /while/ loops
  | ForSt t (VarIdent t) (Args t) (Prog t)
  -- ^ /for/ loops
  | IfSt t [(Stmt t,Prog t)] (Maybe (Prog t))
  -- ^ /if/ statements
  | SwitchSt t (Expr t) [(Expr t,Prog t)]
  -- ^ /switch/ statements
  | BeginSt t (Prog t)
  -- ^ /begin/ statements
  | AndSt t (Stmt t)
  -- ^ /and/ statement modifier
  | OrSt t (Stmt t)
  -- ^ /or/ statement modifier
  | NotSt t (Stmt t)
  -- ^ /not/ statement modifier
  | RedirectedSt t (Stmt t) [Redirect t]
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
  -- ^ Braces expansion, i.e. (..).
  | ConcatE t (Expr t) (Expr t)
  -- ^ One expression following the other without seperating whitespace.
  deriving (Eq,Ord,Show,Functor)


-- | A (std) file descriptor, can be either inwards (only stdin)
--   or outwards (stdout,stderr)
type Fd = Either InFd OutFd

-- | Stdin file descriptor.
data InFd = StdInFd
  deriving (Eq,Ord,Show)

-- | Stdout / stderr file descriptor.
data OutFd =
  StdOutFd
  | StdErrFd
  deriving (Eq,Ord,Show)

-- | Glob pattern, can be one of * ** ?
data Glob = 
  StarGl
  | DiStarGl
  | QMarkGl
  deriving (Eq,Ord,Show)

-- | Variable identifiers
data VarIdent t = VarIdent t S
  deriving (Eq,Ord,Show,Functor)

-- | Function identifiers
data FunIdent t = FunIdent t S
  deriving (Eq,Ord,Show,Functor)

-- | Command name identifiers
data CmdIdent t = CmdIdent t S
  deriving (Eq,Ord,Show,Functor)

-- | Type of a redirection, the first file descriptor
--   is the fd being redirected, the second part is
--   the target. It can be either another fd or a file,
--   in which case the boolean tells us whether it should
--   be overwritten (False) or appended to (True).
data Redirect t = Redirect Fd ( Either Fd (Append,Expr t) )
  deriving (Eq,Ord,Show,Functor)

type Append = Bool

-- | An Index from an [..] index expression.
--   Can be either a single index or an index range.
data Indexing i = Index i | Range i i
  deriving (Eq,Ord,Show,Functor)

-- | An optional index expression, consisting
--   of a list of indices / index ranges.
type Ref i = Maybe [Indexing i]

-- | A variable reference starting with a name,
--   which may be either another variable reference
--   or an variable identifier, potentially followed
--   by an index expression.
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
--   the set builtin. The only difference from 'VarRef'
--   is that the name must be a statically known identifier.
data VarDef t = VarDef t
  ( VarIdent t )
  ( Ref (Expr t) )
  deriving (Eq,Ord,Show,Functor)

-- | A command reference, given by a piece of fish code and
--   an optional index expression.
data CmdRef t = CmdRef t (Prog t) (Ref (Expr t))
  deriving (Eq,Ord,Show,Functor)

