{-# language LambdaCase, FlexibleInstances #-}
module Fish.QuickCheck.Arbitrary where

import Test.QuickCheck
import Fish.Lang
import qualified Data.Text as T
import qualified Data.Char as C
import Data.NText

genVarIdent = mkNText . T.pack
  <$> ( listOf1
        ( arbitrary `suchThat` f)
        `suchThat` ( C.isLetter . head ) )
  where
    f c = C.isAlphaNum c || (=='_') c

genFunIdent = mkNText . T.pack
  <$> ( listOf1
        ( arbitrary `suchThat` C.isAlphaNum )
        `suchThat` ( C.isLetter . head ) )
  where
    f c = C.isAlphaNum c || (=='_') c || (=='-') c

genCmdIdent = mkNText . T.pack
  <$> ( listOf1
        ( arbitrary `suchThat` C.isAlphaNum )
        `suchThat` ( C.isLetter . head ) )
  where
    f c = C.isAlphaNum c || (=='_') c || (=='-') c || (=='/') c

instance Arbitrary t => Arbitrary (Prog T.Text t) where
  arbitrary = Prog <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (Exprs T.Text t) where
  arbitrary = Exprs <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (CompStmt T.Text t) where
  arbitrary = scale (`div`2) $ oneof
    [ Simple <$> arbitrary <*> arbitrary
      ,Piped <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ,Forked <$> arbitrary <*> arbitrary ]

instance Arbitrary t => Arbitrary (Stmt T.Text t) where
  arbitrary = scale (`div`2) $ oneof
    [ CommentSt <$> arbitrary <*> arbitrary
      ,CmdSt <$> arbitrary <*> arbitrary
      ,FunctionSt <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ,WhileSt <$> arbitrary <*> arbitrary <*> arbitrary
      ,ForSt <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ,IfSt <$> arbitrary <*> arbitrary <*> arbitrary
      ,SwitchSt <$> arbitrary <*> arbitrary <*> arbitrary
      ,BeginSt <$> arbitrary <*> arbitrary
      ,AndSt <$> arbitrary <*> arbitrary
      ,OrSt <$> arbitrary <*> arbitrary
      ,NotSt <$> arbitrary <*> arbitrary
      ,RedirectedSt <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary t => Arbitrary (Expr T.Text t) where
  arbitrary = scale (`div`2) $ oneof
    [ StringE <$> arbitrary <*> arbitrary
      ,GlobE <$> arbitrary <*> arbitrary
      ,HomeDirE <$> arbitrary
      ,VarRefE <$> arbitrary <*> arbitrary <*> arbitrary
      ,BracesE <$> arbitrary <*> arbitrary
      ,CmdSubstE <$> arbitrary <*> arbitrary
      ,ConcatE <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Glob where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary t => Arbitrary (VarIdent T.Text t) where
  arbitrary = VarIdent <$> arbitrary <*> genVarIdent

instance Arbitrary t => Arbitrary (FunIdent T.Text t) where
  arbitrary = FunIdent <$> arbitrary <*> genFunIdent

instance Arbitrary t => Arbitrary (CmdIdent T.Text t) where
  arbitrary = CmdIdent <$> arbitrary <*> genCmdIdent

instance Arbitrary Fd where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FileMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary e => Arbitrary (Redirect e) where
  arbitrary = scale (`div`2) $ do
    fd <- arbitrary
    oneof
      [ return (RedirectClose fd)
        ,RedirectIn fd <$> arbitrary
        ,RedirectOut fd <$> arbitrary ]

instance Arbitrary i => Arbitrary (Indexing i) where
  arbitrary = scale (`div`2) $ oneof
    [ Index <$> arbitrary
      ,Range <$> arbitrary <*> arbitrary ]

instance Arbitrary t => Arbitrary (VarRef T.Text t) where
  arbitrary = scale (`div`2)
    (VarRef <$> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary t => Arbitrary (VarDef T.Text t) where
  arbitrary = scale (`div`2)
    (VarDef <$> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary t => Arbitrary (CmdRef T.Text t) where
  arbitrary = scale (`div`2) 
    (CmdRef <$> arbitrary <*> arbitrary <*> arbitrary)

