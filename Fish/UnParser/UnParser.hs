{-# language LambdaCase, OverloadedStrings #-}
module Fish.UnParser.UnParser where

import Data.Monoid
import Data.String (fromString)
import qualified Data.List.NonEmpty as N
import Control.Applicative
import Fish.Lang
import Fish.UnParser.Quote

quote = quoteSQ

mintcal :: Monoid m => m -> [m] -> m
mintcal s = \case
  [] -> mempty
  xs -> foldr1 (\x y -> x <> s <> y) xs

unLines = mintcal "\n"
unWords = mintcal " "

class Unparse a where
  unparse :: a -> Str
  
  unparseLn :: a -> Str
  unparseLn a = unparse a <> "\n"
  
  unparseSp :: a -> Str
  unparseSp a = unparse a <> " "

instance Unparse (Prog t) where
  unparse (Prog _ sts) = 
    mconcat $ map unparseLn sts

instance Unparse (Args t) where
  unparse (Args _ es) = 
    unWords (map unparse es)

instance Unparse (CompStmt t) where
  unparse = \case
    Simple _ st -> unparse st
    Piped _ pfd st cst ->
      unparseSp st
      <> unparsePipe pfd <> " "
      <> unparse cst
    Forked t st -> unparse st <> " &"

unparsePipe :: Fd -> Str
unparsePipe fd =
  unparseFdL True fd <> "|"

instance Unparse (Stmt t) where
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

unparseCommentSt :: Str -> Str
unparseCommentSt = ("#" <>)

unparseCmdSt :: CmdIdent t -> Args t -> Str
unparseCmdSt cmdi args =
  unparseSp cmdi
  <> unparse args

instance Unparse t => Unparse (SetCommand t) where
  unparse = unparseSetSt

unparseSetSt :: SetCommand t -> Str
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

unparseFunctionSt :: FunIdent t -> Args t -> Prog t -> Str
unparseFunctionSt funi args prog =
  "function" <> " "
  <> unparseSp funi
  <> unparseLn args
  <> unparseLn prog
  <> "end"

unparseWhileSt :: Stmt t -> Prog t -> Str
unparseWhileSt st prog =
  "while" <> " "
  <> unparseLn st
  <> unparseLn prog
  <> "end"

unparseForSt :: VarIdent t -> Args t -> Prog t -> Str
unparseForSt vari args prog =
  "for" <> " "
  <> unparseSp vari
  <> "in" <> " "
  <> unparseLn args
  <> unparseLn prog
  <> "end"

unparseIfSt :: N.NonEmpty (Stmt t,Prog t) -> Maybe (Prog t) -> Str
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

unparseSwitchSt :: Expr t -> N.NonEmpty (Expr t,Prog t) -> Str
unparseSwitchSt e cases =
  "switch" <> " " <> unparseLn e
  <> unLines (map unparseCase $ N.toList cases)
  <> "\n" <> "end"
  where
    unparseCase (e,prog) =
      "case" <> " " <> unparseLn e
      <> unparseLn prog

unparseBeginSt :: Prog t -> Str
unparseBeginSt prog = 
  "begin" <> " " <> "\n"
  <> unparseLn prog
  <> "end"

unparseAndSt :: Stmt t -> Str
unparseAndSt st =
  "and" <> " " <> unparse st

unparseOrSt :: Stmt t -> Str
unparseOrSt st =
  "or" <> " " <> unparse st

unparseNotSt :: Stmt t -> Str
unparseNotSt st =
  "not" <> " " <> unparse st

unparseRedirectedSt :: Stmt t -> N.NonEmpty (Redirect t) -> Str
unparseRedirectedSt st redirs =
  unparseSp st
  <> unWords (map unparse $ N.toList redirs)


instance Unparse (Expr t) where
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

instance Unparse (CmdRef t) where
  unparse (CmdRef _ (Prog _ sts) ref) = 
    "(" <> mintcal " ; " (map unparse sts) <> ")"
    <> unparseRef ref

instance Unparse (VarDef t) where
  unparse (VarDef _ name ref) = 
    unparse name
    <> unparseRef ref

instance Unparse (VarRef t) where
  unparse (VarRef _ name ref) = 
    "$" <>
    either unparse unparse name
    <> unparseRef ref

unparseRef :: Unparse i => Ref i -> Str
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

instance Unparse (VarIdent t) where
  unparse (VarIdent _ s) = s

instance Unparse (FunIdent t) where
  unparse (FunIdent _ s) = s

instance Unparse (CmdIdent t) where
  unparse (CmdIdent _ s) = s

instance Unparse (Redirect t) where
  unparse = unparseRedirect

unparseRedirect :: Redirect t -> Str
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

unparseFdL :: Bool -> Fd -> Str
unparseFdL out fd =
  (fromString . show)
    (fromEnum fd)
  <> if out then ">" else "<"

unparseFdR :: Fd -> Str
unparseFdR fd =
  "&" <>
  (fromString . show)
    (fromEnum fd)


