{-# LANGUAGE DeriveFunctor #-}
module Fish.Lang.Lang where

import qualified Data.Text as T
import Data.Bifunctor

data Prog t = Prog t [CompStmt t]
  deriving (Eq,Ord,Show,Functor)

data Args t = Args t [Expr t]
  deriving (Eq,Ord,Show,Functor)

data CompStmt t =
  Simple t (Stmt t)
  | Piped t OutFd (Stmt t) (CompStmt t)
  | Forked t (Stmt t)
  deriving (Eq,Ord,Show,Functor)

data Stmt t = 
  CommentSt t S
  | CmdSt t (CmdIdent t) (Args t)
  | SetSt t (Maybe ((VarDef t),(Args t)))
  | FunctionSt t (FunIdent t) (Args t) (Prog t)
  | WhileSt t (Stmt t) (Prog t)
  | ForSt t (VarIdent t) (Args t) (Prog t)
  | IfSt t [(Stmt t,Prog t)] (Maybe (Prog t))
  | SwitchSt t (Expr t) [(Expr t,Prog t)]
  | BeginSt t (Prog t)
  | AndSt t (Stmt t)
  | OrSt t (Stmt t)
  | NotSt t (Stmt t)
  | RedirectedSt t (Stmt t) [Redirect t]
  deriving (Eq,Ord,Show,Functor)

data Expr t =
  StringE t S
  | GlobE t Glob
  | ProcE t (Expr t)
  | HomeDirE t
  | VarRefE t Bool (VarRef t)
  | BracesE t [Expr t]
  | CmdSubstE t (CmdRef t)
  | ConcatE t (Expr t) (Expr t)
  deriving (Eq,Ord,Show,Functor)

type Fd = Either InFd OutFd

data InFd = StdInFd
  deriving (Eq,Ord,Show)

data OutFd =
  StdOutFd
  | StdErrFd
  deriving (Eq,Ord,Show)

data Glob = 
  StarGl
  | DiStarGl
  | QMarkGl
  deriving (Eq,Ord,Show)

type S = T.Text

data VarIdent t = VarIdent t S
  deriving (Eq,Ord,Show,Functor)

data FunIdent t = FunIdent t S
  deriving (Eq,Ord,Show,Functor)

data CmdIdent t = CmdIdent t S
  deriving (Eq,Ord,Show,Functor)

data Redirect t = Redirect Fd ( Either Fd (Append,Expr t) )
  deriving (Eq,Ord,Show,Functor)

type Append = Bool

data Indexing i = Index i | Range i i
  deriving (Eq,Ord,Show,Functor)

type Ref i = Maybe [Indexing i]

data VarRef t = VarRef t
  ( Either (VarRef t) (VarIdent t) )
  ( Ref (Expr t) )
  deriving (Eq,Ord,Show)

instance Functor VarRef where
  fmap f (VarRef t a b) = 
    VarRef (f t)
    ( bimap (fmap f) (fmap f) a )
    ( fmap (map (fmap $ fmap f)) b )

data VarDef t = VarDef t
  ( VarIdent t )
  ( Ref (Expr t) )
  deriving (Eq,Ord,Show,Functor)

data CmdRef t = CmdRef t (Prog t) (Ref (Expr t))
  deriving (Eq,Ord,Show,Functor)

