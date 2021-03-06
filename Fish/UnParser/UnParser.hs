{-# language LambdaCase, OverloadedStrings, FlexibleInstances #-}
module Fish.UnParser.UnParser where

import Data.NText
import Data.Semigroup
import Data.String (fromString,IsString)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as N
import qualified Data.Foldable as F
import Control.Applicative
import Fish.Lang
import Fish.UnParser.Quote

quote = quoteSQ

mintcal :: (Foldable t,Monoid m) => m -> t m -> m
mintcal s ms = case F.toList ms of
  [] -> mempty
  xs -> foldr1 (\x y -> x <> s <> y) xs

unLines :: (Foldable t,Monoid m,IsString m) => t m -> m
unLines = mintcal $ fromString "\n"

unWords :: (Foldable t,Monoid m,IsString m) => t m -> m
unWords = mintcal $ fromString " "

class Unparse a where
  unparse :: a -> T.Text
  
  unparseLn :: a -> T.Text
  unparseLn a = unparse a <> "\n"
  
  unparseSp :: a -> T.Text
  unparseSp a = unparse a <> " "

instance Unparse (Prog T.Text t) where
  unparse (Prog _ sts) = 
    mconcat $ map unparseLn sts

instance Unparse (Exprs T.Text t) where
  unparse (Exprs _ es) = 
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
    CmdSt _ exprs ->
      unparseCmdSt exprs
    FunctionSt _ funi exprs prog ->
      unparseFunctionSt funi exprs prog
    WhileSt _ st prog ->
      unparseWhileSt st prog
    ForSt _ vari exprs prog ->
      unparseForSt vari exprs prog
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

unparseCmdSt :: N.NonEmpty (Expr T.Text t) -> T.Text
unparseCmdSt exprs = unWords (fmap unparse exprs)

unparseFunctionSt :: FunIdent T.Text t -> Exprs T.Text t -> Prog T.Text t -> T.Text
unparseFunctionSt funi exprs prog =
  "function" <> " "
  <> unparseSp funi
  <> unparseLn exprs
  <> unparseLn prog
  <> "end"

unparseWhileSt :: Stmt T.Text t -> Prog T.Text t -> T.Text
unparseWhileSt st prog =
  "while" <> " "
  <> unparseLn st
  <> unparseLn prog
  <> "end"

unparseForSt :: VarIdent T.Text t -> Exprs T.Text t -> Prog T.Text t -> T.Text
unparseForSt vari exprs prog =
  "for" <> " "
  <> unparseSp vari
  <> "in" <> " "
  <> unparseLn exprs
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

instance Unparse (VarIdent T.Text t) where
  unparse (VarIdent _ s) = extractText s

instance Unparse (FunIdent T.Text t) where
  unparse (FunIdent _ s) = extractText s

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


