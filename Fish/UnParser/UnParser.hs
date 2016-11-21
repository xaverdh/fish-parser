{-# language LambdaCase, OverloadedStrings #-}
module Fish.UnParser.UnParser where

import Data.Monoid
import Control.Applicative
import Fish.Lang.Lang
import Fish.UnParser.Quote

quote = quoteSQ

mintCal :: Monoid m => m -> [m] -> m
mintCal s = \case
  [] -> mempty
  xs -> foldr1 (\x y -> x <> s <> y) xs

class Unparse a where
  unparse :: a -> S


instance Unparse (Prog t) where
  unparse (Prog _ sts) = 
    mintCal "\n" (map unparse sts)

instance Unparse (Args t) where
  unparse (Args _ es) = 
    mintCal " " (map unparse es)

instance Unparse (CompStmt t) where
  unparse = \case
    Simple _ st -> unparse st
    Piped _ pfd st cst ->
      unparse st <> " "
      <> unparsePipe pfd <> " "
      <> unparse cst
    Forked t st -> unparse st <> " &"

unparsePipe :: OutFd -> S
unparsePipe = \case
  StdOutFd -> "|"
  StdErrFd -> "2>|"

instance Unparse (Stmt t) where
  unparse = \case
    CommentSt _ s ->
      unparseCommentSt s
    CmdSt _ cmdi args ->
      unparseCmdSt cmdi args
    SetSt _ mdef -> unparseSetSt mdef
    FunctionSt _ funi args prog ->
      unparseFunctionSt funi args prog
    WhileSt _ st prog ->
      unparseWhileSt st prog
    ForSt _ vari args prog ->
      unparseForSt vari args prog
    IfSt _ clauses mfinal ->
      unparseIfSt clauses mfinal
    SwitchSt _ e cases ->
      unparseSwitchSt e cases
    BeginSt _ prog -> 
      unparseBeginSt prog
    AndSt _ st ->
      unparseAndSt st
    OrSt _ st ->
      unparseOrSt st
    NotSt _ st ->
      unparseNotSt st
    RedirectedSt _ st redirs ->
      unparseRedirectedSt st redirs

unparseCommentSt :: S -> S
unparseCommentSt = ("#" <>)

unparseCmdSt :: CmdIdent t -> Args t -> S
unparseCmdSt cmdi args =
  unparse cmdi <> " "
  <> unparse args

unparseSetSt :: Maybe (VarDef t,Args t) -> S
unparseSetSt = \case
  Nothing -> "set"
  Just (vdef,args) ->
    "set" <> " "
    <> unparse vdef <> " " <> unparse args

unparseFunctionSt :: FunIdent t -> Args t -> Prog t -> S
unparseFunctionSt funi args prog =
  "function" <> " "
  <> unparse funi <> " "
  <> unparse args <> "\n"
  <> unparse prog <> "\n" <> "end"

unparseWhileSt :: Stmt t -> Prog t -> S
unparseWhileSt st prog =
  "while" <> " "
  <> unparse st <> "\n"
  <> unparse prog <> "\n" <> "end"

unparseForSt :: VarIdent t -> Args t -> Prog t -> S
unparseForSt vari args prog =
  "for" <> " "
  <> unparse vari <> " "
  <> "in" <> " "
  <> unparse args <> "\n"
  <> unparse prog <> "\n" <> "end"

unparseIfSt :: [(Stmt t,Prog t)] -> Maybe (Prog t) -> S
unparseIfSt clauses mfinal =
  mintCal ("\n" <> "else" <> " ")
    (map unparseClause clauses)
  <> maybe "" unparseFinal mfinal
  <> "\n" <> "end"
  where
    unparseClause (st,prog) =
      "if" <> " " <> unparse st <> "\n"
      <> unparse prog
    unparseFinal prog =
      "else" <> "\n"
      <> unparse prog

unparseSwitchSt :: Expr t -> [(Expr t,Prog t)] -> S
unparseSwitchSt e cases =
  "switch" <> " " <> unparse e <> "\n"
  <> mintCal "\n" (map unparseCase cases)
  <> "\n" <> "end"
  where
    unparseCase (e,prog) =
      "case" <> " " <> unparse e <> "\n"
      <> unparse prog <> "\n"

unparseBeginSt :: Prog t -> S
unparseBeginSt prog = 
  "begin" <> " " <> "\n"
  <> unparse prog
  <> "\n" <> "end"

unparseAndSt :: Stmt t -> S
unparseAndSt st =
  "and" <> " " <> unparse st

unparseOrSt :: Stmt t -> S
unparseOrSt st =
  "or" <> " " <> unparse st

unparseNotSt :: Stmt t -> S
unparseNotSt st =
  "not" <> " " <> unparse st

unparseRedirectedSt :: Stmt t -> [Redirect t] -> S
unparseRedirectedSt st redirs =
  unparse st <> " "
  <> mintCal " " (map unparse redirs)


instance Unparse (Expr t) where
  unparse = \case
    StringE _ s -> quote s
    GlobE _ g -> unparse g
    ProcE _ e -> unparse e
    HomeDirE _ -> "~"
    VarRefE _ q vref -> unparse vref
    BracesE _ es -> "{" <> mintCal "," (map unparse es) <> "}"
    CmdSubstE _ cref -> unparse cref
    ConcatE _ e1 e2 -> unparse e1 <> " " <> unparse e2

instance Unparse (CmdRef t) where
  unparse (CmdRef _ (Prog _ sts) ref) = 
    "(" <> mintCal " ; " (map unparse sts) <> ")"
    <> unparseRef ref

instance Unparse (VarDef t) where
  unparse (VarDef _ name ref) = 
    unparse name
    <> unparseRef ref

instance Unparse (VarRef t) where
  unparse (VarRef _ name ref) = 
    either unparse unparse name
    <> unparseRef ref

unparseRef :: Unparse i => Ref i -> S
unparseRef = maybe ""
  (bracket . mintCal " " . map unparse)
  where
    bracket t = "[" <> t <> "]"

instance Unparse i => Unparse (Indexing i) where
  unparse = \case
    Index a -> unparse a
    Range a b -> unparse a <> ".." <> unparse b

instance Unparse Glob where
  unparse = \case
    StarGl -> "*"
    DiStarGl -> "**"
    QMarkGl -> "?"

instance Unparse (VarIdent t) where
  unparse (VarIdent _ s) = s

instance Unparse (FunIdent t) where
  unparse (FunIdent _ s) = s

instance Unparse (CmdIdent t) where
  unparse (CmdIdent _ s) = s

instance Unparse (Redirect t) where
  unparse = unparseRedirect

unparseRedirect :: Redirect t -> S
unparseRedirect (Redirect l r) = 
  case l of
    Left StdInFd -> "<"
      <> case r of
        Left fd -> " " <> unparseFd fd
        Right (False,e) -> " " <> unparse e
        Right (True,_) ->
          error "Found invalid redirection."
    Right StdOutFd -> ">"
      <> case r of
        Left fd -> " " <> unparseFd fd
        Right (app,e) ->
          (if app then ">" else "")
          <> " " <> unparse e
    Right StdErrFd -> "^" 
      <> case r of
        Left fd -> " " <> unparseFd fd
        Right (app,e) ->
          (if app then "^" else "")
          <> " " <> unparse e
  where
    unparseFd = \case
      Left StdInFd -> "&0"
      Right StdOutFd -> "&1"
      Right StdErrFd -> "&2"

