{-# language LambdaCase, OverloadedStrings, FlexibleInstances #-}
module Fish.UnParser.UnParser where

import Data.NText
import Data.Semigroup
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as N
import Control.Applicative
import Fish.Lang
import Fish.UnParser.Quote

quote = quoteSQ

mintcal :: (Semigroup m,Monoid m) => m -> [m] -> m
mintcal s = \case
  [] -> mempty
  xs -> foldr1 (\x y -> x <> s <> y) xs

unLines = mintcal "\n"
unWords = mintcal " "

class Unparse a where
  unparse :: a -> T.Text
  
  unparseLn :: a -> T.Text
  unparseLn a = unparse a <> "\n"
  
  unparseSp :: a -> T.Text
  unparseSp a = unparse a <> " "

instance Unparse (Prog T.Text t) where
  unparse (Prog _ sts) = 
    mconcat $ map unparseLn sts

instance Unparse (Args T.Text t) where
  unparse (Args _ es) = 
    unWords (map unparse es)

instance Unparse (CompStmt T.Text t) where
  unparse = \case
    Simple _ st -> unparse st
    Piped _ pfd st cst ->
      unparseSp st
      <> unparsePipe pfd <> " "
      <> unparse cst
    Forked t st -> unparse st <> " &"

unparsePipe :: Fd -> T.Text
unparsePipe fd =
  unparseFdL True fd <> "|"

instance Unparse (Stmt T.Text t) where
  unparse = \case
    CommentSt _ s ->
      unparseCommentSt s
    CmdSt _ cmdi args ->
      unparseCmdSt cmdi args
    SetSt _ setcmd -> unparseSetSt setcmd
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

unparseCommentSt :: T.Text -> T.Text
unparseCommentSt = ("#" <>)

unparseCmdSt :: CmdIdent T.Text t -> Args T.Text t -> T.Text
unparseCmdSt cmdi args =
  unparseSp cmdi
  <> unparse args

instance Unparse t => Unparse (SetCommand T.Text t) where
  unparse = unparseSetSt

unparseSetSt :: SetCommand T.Text t -> T.Text
unparseSetSt = (("set" <> " ") <>) . \case
  SetSetting mscope mexport vdef args ->
    unparseMScope mscope
    <> unparseMExport mexport
    <> unparseSp vdef
    <> unparse args
  SetList mscope mexport namesOnly ->
    if namesOnly then "-n" <> " " else ""
    <> unparseMExport mexport
    <> unparseMScope mscope
  SetQuery mscope mexport args ->
    "-q" <> " "
    <> unparseMScope mscope
    <> unparseMExport mexport
    <> unparse args
  SetErase mscope vdefs ->
    "-e" <> " "
    <> unparseMScope mscope
    <> unWords (map unparse $ N.toList vdefs)
  SetHelp -> "--help"
  where
    unparseMScope = maybe "" unparseSp
    unparseMExport = maybe "" unparseSp

instance Unparse Scope where
  unparse = \case
    ScopeLocal -> "-l"
    ScopeGlobal -> "-g"
    ScopeUniversal -> "-U"

instance Unparse Export where
  unparse = \case
    Export -> "-x"
    UnExport -> "-u"

unparseFunctionSt :: FunIdent T.Text t -> Args T.Text t -> Prog T.Text t -> T.Text
unparseFunctionSt funi args prog =
  "function" <> " "
  <> unparseSp funi
  <> unparseLn args
  <> unparseLn prog
  <> "end"

unparseWhileSt :: Stmt T.Text t -> Prog T.Text t -> T.Text
unparseWhileSt st prog =
  "while" <> " "
  <> unparseLn st
  <> unparseLn prog
  <> "end"

unparseForSt :: VarIdent T.Text t -> Args T.Text t -> Prog T.Text t -> T.Text
unparseForSt vari args prog =
  "for" <> " "
  <> unparseSp vari
  <> "in" <> " "
  <> unparseLn args
  <> unparseLn prog
  <> "end"

unparseIfSt :: N.NonEmpty (Stmt T.Text t,Prog T.Text t) -> Maybe (Prog T.Text t) -> T.Text
unparseIfSt clauses mfinal =
  mintcal ("\n" <> "else" <> " ")
    (map unparseClause $ N.toList clauses)
  <> maybe "" unparseFinal mfinal
  <> "\n" <> "end"
  where
    unparseClause (st,prog) =
      "if" <> " " <> unparseLn st
      <> unparse prog
    unparseFinal prog =
      "else" <> "\n"
      <> unparse prog

unparseSwitchSt :: Expr T.Text t -> N.NonEmpty (Expr T.Text t,Prog T.Text t) -> T.Text
unparseSwitchSt e cases =
  "switch" <> " " <> unparseLn e
  <> unLines (map unparseCase $ N.toList cases)
  <> "\n" <> "end"
  where
    unparseCase (e,prog) =
      "case" <> " " <> unparseLn e
      <> unparseLn prog

unparseBeginSt :: Prog T.Text t -> T.Text
unparseBeginSt prog = 
  "begin" <> " " <> "\n"
  <> unparseLn prog
  <> "end"

unparseAndSt :: Stmt T.Text t -> T.Text
unparseAndSt st =
  "and" <> " " <> unparse st

unparseOrSt :: Stmt T.Text t -> T.Text
unparseOrSt st =
  "or" <> " " <> unparse st

unparseNotSt :: Stmt T.Text t -> T.Text
unparseNotSt st =
  "not" <> " " <> unparse st

unparseRedirectedSt :: Stmt T.Text t
  -> N.NonEmpty (Redirect (Expr T.Text t)) -> T.Text
unparseRedirectedSt st redirs =
  unparseSp st
  <> unWords (map unparse $ N.toList redirs)


instance Unparse (Expr T.Text t) where
  unparse = \case
    StringE _ s -> quote s
    GlobE _ g -> unparse g
    ProcE _ e -> "%" <> unparse e
    HomeDirE _ -> "~"
    VarRefE _ q vref -> 
      if q 
        then "\"" <> unparse vref <> "\""
        else unparse vref
    BracesE _ es -> "{" <> mintcal "," (map unparse es) <> "}"
    CmdSubstE _ cref -> unparse cref
    ConcatE _ e1 e2 -> unparse e1 <> unparse e2

instance Unparse (CmdRef T.Text t) where
  unparse (CmdRef _ (Prog _ sts) ref) = 
    "(" <> mintcal " ; " (map unparse sts) <> ")"
    <> unparseRef ref

instance Unparse (VarDef T.Text t) where
  unparse (VarDef _ name ref) = 
    unparse name
    <> unparseRef ref

instance Unparse (VarRef T.Text t) where
  unparse (VarRef _ name ref) = 
    "$" <>
    either unparse unparse name
    <> unparseRef ref

unparseRef :: Unparse i => Ref i -> T.Text
unparseRef = bracket
  . maybe "1..-1" (unWords . map unparse)
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

instance Unparse (VarIdent T.Text t) where
  unparse (VarIdent _ s) = extractText s

instance Unparse (FunIdent T.Text t) where
  unparse (FunIdent _ s) = extractText s

instance Unparse (CmdIdent T.Text t) where
  unparse (CmdIdent _ s) = extractText s

instance Unparse e => Unparse (Redirect e) where
  unparse = unparseRedirect

unparseRedirect :: Unparse e => Redirect e -> T.Text
unparseRedirect = \case
  RedirectClose fd ->
    unparseFdL True fd <> "&-"
  RedirectIn fdl r ->
    unparseFdL False fdl
    <> case r of
      Left fdr -> unparseFdR fdr
      Right e -> " " <> unparse e
  RedirectOut fdl r ->
    unparseFdL True fdl
    <> case r of
      Left fdr -> unparseFdR fdr
      Right (mode,e) -> (<> " " <> unparse e)
        $ case mode of
          FModeWrite -> ""
          FModeApp -> ">"
          FModeNoClob -> "?"

unparseFdL :: Bool -> Fd -> T.Text
unparseFdL out fd =
  (fromString . show)
    (fromEnum fd)
  <> if out then ">" else "<"

unparseFdR :: Fd -> T.Text
unparseFdR fd =
  "&" <>
  (fromString . show)
    (fromEnum fd)


