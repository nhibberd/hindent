{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | A module comment. This module contains obnoxiously-formatted code from
-- a variety of arbitrary locations. Cheers
module Some.Module.Name (fun, Ty (..), name, abcde, module X, F.bar) where

-- unqualified imports should be padded

import AST

import Control.Monad.Except

import Data.Map.Strict hiding (Map, undefined, undefined, undefined, undefined, undefined, undefined, undefined, Abc, Def, Ghi, JKL)


import P

import qualified Prelude

{- These ugly imports are not worth padding -}
import {-# SOURCE #-} A (TA(..))
import qualified "network" Network.Socket as NN
import {-# SOURCE #-} safe qualified "network" Network.Socket as N (A, B, C)

foo :: Bool
foo =
  if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa then foo else
    baz


thing :: Read Thing
thing = Read.bounded thing

newtype Thing = Thing { fromThing :: Text }
    deriving (Show, Read, Eq, ToJSON)

data Config =
  Config {
      configA :: A
    , bonfigB :: B
    } deriving (Eq, Show)


data Baz
    = Foo
    | Woah
    | Blahfoo
      deriving Eq

{--
newtype Thing =ss Thing
  { fromThing :: Text
  } deriving (Show, Read, Eq, ToJSON)

data Config =ss Config
  { configA :: A
  , bonfigB :: B
  } deriving (Eq, Show)

data Baz
  = Foo
  | Woah
  | Blahfoo
  deriving (Eq)
--}

class A a where
    aaa :: proxy a -> Baz

instance A Foo     where aaa = const Foo
instance A Woah    where aaa = const Woah
instance A Blahfoo where aaa = const Woah



data TypeError = Mismatch Expr Type Type
               | UnknownType Var
               | SomethingHappening Type Typical Typified Mulberries LongCon ConDecl WhatSayYou (WhatSay You Huh)
               | ExpectedFunction Expr Type Type deriving (Eq, Show)

newtype Context = Context { unContext :: Map Var Type } deriving (Eq, Show)

mkContext :: [(Var, Type)] -> Context
mkContext = Context . M.fromList

longSigLongType :: (MyConstraint a, YourConstraint b) => Type -> This Is A Long Type (What Should Happen To It) Really When You (Think About) It I Dunno Hey (How About) That -> Second Type -> Eight Type -> Maybe (EitherT IO m) a b c d
longSigLongType = undefined


data Foo =
    Foo Foo
  | Bar Foo
  | BazSnaffle
      Foo
      Bar
      Baz
      Int
      Double
      Double
      [Double]
  | Quux

foo ::
  Foo ->
  Bar ->
  Int ->
  EitherT
    (Foo Bar Baz)
    IO
    (Conduit Foo IO (Bar Baz Snaffle Quux))



longTypeSig :: (MyConstraint a, YourConstraint b) => Def -> Ghi -> Jkl -> Abcdeoijoijoij -> Abcoiwjefiojwoeif -> Jklwejfiowejf -> Lowepfwpef
longTypeSig abc (Def ghi jkl mno) = case x of
   undefined -> unsafePerformIO $ undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined
    where abcde :: Integer -> Integer -> Integer
          abcde = 55 55 55 55 55 55
          gumbus = aoij oijaw o foa wefi jawoeijf aoweijf oija wefoij aweofj awoef joija wefj aweofij awoiejf awoijef aowiejf aoiwejf

-- | Pretty printing class.
class (Annotated ast,Typeable ast) => Pretty ast where
  prettyInternal :: ast NodeInfo -> Printer ()

-- | Pretty print including comments.
pretty :: (Pretty ast,Show (ast NodeInfo))
       => ast NodeInfo -> Printer ()
pretty a = do
  mapM_
    (\c' -> do
       case c' of
         CommentBeforeLine c -> do
           case c of
             EndOfLine s -> write ("--" ++ s)
             MultiLine s -> write ("{-" ++ s ++ "-}")
           newline
         _ -> return ())
    comments
  prettyInternal a
  mapM_
    (\(i, c') -> do
       case c' of
         CommentSameLine c -> do
           col <- gets psColumn
           unless (col == 0) space
           writeComment c
         CommentAfterLine c -> do
           when (i == 0) newline
           writeComment c
         _ -> return ())
    (zip [0 :: Int ..] comments)
  where
    comments = nodeInfoComments (ann a)
    writeComment =
      \case
        EndOfLine cs -> do
          write ("--" ++ cs)
          modify
            (\s ->
                s
                { psEolComment = True
                })
        MultiLine cs -> do
          write ("{-" ++ cs ++ "-}")
          modify
            (\s ->
                s
                { psEolComment = True
                })
