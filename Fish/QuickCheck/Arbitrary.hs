{-# language LambdaCase #-}
module Fish.QuickCheck.Arbitrary where

import Test.QuickCheck hiding (Args)
import Fish.Lang.Lang
import qualified Data.Text as T
import qualified Data.Char as C

genVarIdent = T.pack
  <$> ( listOf1
        ( arbitrary `suchThat` f)
        `suchThat` ( C.isLetter . head ) )
  where
    f c = C.isAlphaNum c || (=='_') c

genFunIdent = T.pack
  <$> ( listOf1
        ( arbitrary `suchThat` C.isAlphaNum )
        `suchThat` ( C.isLetter . head ) )
  where
    f c = C.isAlphaNum c || (=='_') c || (=='-') c

genCmdIdent = T.pack
  <$> ( listOf1
        ( arbitrary `suchThat` C.isAlphaNum )
        `suchThat` ( C.isLetter . head ) )
  where
    f c = C.isAlphaNum c || (=='_') c || (=='-') c || (=='/') c

instance Arbitrary t => Arbitrary (Prog t) where
  arbitrary = Prog <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (Args t) where
  arbitrary = Args <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (CompStmt t) where
  arbitrary = scale (`div`2) $ oneof
    [ Simple <$> arbitrary <*> arbitrary
      ,Piped <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ,Forked <$> arbitrary <*> arbitrary ]

instance Arbitrary t => Arbitrary (Stmt t) where
  arbitrary = scale (`div`2) $ oneof
    [ CommentSt <$> arbitrary <*> arbitrary
      ,CmdSt <$> arbitrary <*> arbitrary <*> arbitrary
      ,SetSt <$> arbitrary <*> arbitrary
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

instance Arbitrary t => Arbitrary (Expr t) where
  arbitrary = scale (`div`2) $ oneof
    [ StringE <$> arbitrary <*> arbitrary
      ,GlobE <$> arbitrary <*> arbitrary
      ,ProcE <$> arbitrary <*> arbitrary
      ,HomeDirE <$> arbitrary
      ,VarRefE <$> arbitrary <*> arbitrary <*> arbitrary
      ,BracesE <$> arbitrary <*> arbitrary
      ,CmdSubstE <$> arbitrary <*> arbitrary
      ,ConcatE <$> arbitrary <*> exprNoProcE <*> arbitrary ]

instance Arbitrary t => Arbitrary (SetCommand t) where
  arbitrary = oneof
    [ SetSetting <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ,SetList <$> arbitrary <*> arbitrary
      ,SetQuery <$> arbitrary <*> arbitrary <*> arbitrary
      ,SetErase <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary Scope where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Export where
  arbitrary = arbitraryBoundedEnum

exprNoProcE :: Arbitrary t => Gen (Expr t)
exprNoProcE = 
  arbitrary `suchThat` \case
    ProcE _ _ -> False
    _ -> True

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Glob where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary t => Arbitrary (VarIdent t) where
  arbitrary = VarIdent <$> arbitrary <*> genVarIdent

instance Arbitrary t => Arbitrary (FunIdent t) where
  arbitrary = FunIdent <$> arbitrary <*> genFunIdent

instance Arbitrary t => Arbitrary (CmdIdent t) where
  arbitrary = CmdIdent <$> arbitrary <*> genCmdIdent

instance Arbitrary Fd where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FileMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary t => Arbitrary (Redirect t) where
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

instance Arbitrary t => Arbitrary (VarRef t) where
  arbitrary = scale (`div`2)
    (VarRef <$> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary t => Arbitrary (VarDef t) where
  arbitrary = scale (`div`2)
    (VarDef <$> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary t => Arbitrary (CmdRef t) where
  arbitrary = scale (`div`2) 
    (CmdRef <$> arbitrary <*> arbitrary <*> arbitrary)

