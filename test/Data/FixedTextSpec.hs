{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.FixedTextSpec (tests) where

import Data.FixedText 

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.List


import Data.Proxy (Proxy(..))

import GHC.TypeLits (Nat,natVal,KnownNat,Symbol,KnownSymbol,symbolVal)
import qualified Regex.Genex as Genex


tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" (
      \list -> sort (list :: [Int]) == sort (reverse list))
  , QC.testProperty "Fermat's little theorem" (
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0)
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" (
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer))
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" (
      [1, 2, 3] `compare` [1,2] @?= GT)

  -- the following test does not hold
  , testCase "List comparison (same length)" (
      [1, 2, 3] `compare` [1,2,2] @?= LT)
  ]



-- | Arbitrary instance
-- This arbitrary instance takes advantage of the Monoid defined above
instance ( KnownNat     max
         , KnownSymbol  regex) => 
  Arbitrary (FixedText max 0 regex) where

    arbitrary = let regexStr        = symbolVal (Proxy :: Proxy regex)        
                    generatedString = Genex.genexPure [regexStr]

                 in either (const mempty) id <$>
                            QC.elements
                              (fixedTextFromString <$>
                                         generatedString)

  
-- | A few examples to make sure everything works right...
-- Working input
exampleFixedText  :: Either FixedTextErrors (FixedText 30 0 "[[:alnum:]]")
exampleFixedText = fixedTextFromString "exampleText1234" 

-- | Cut off too much input.
exampleOverFlowProtection :: Either FixedTextErrors (FixedText 10 1 "[[:alnum:]]")
exampleOverFlowProtection = fixedTextFromString "exampleText1234" 

-- | Reject if below min input
exampleUnderFlowProtection :: Either FixedTextErrors (FixedText 200 20 "[[:alnum:]]")
exampleUnderFlowProtection = fixedTextFromString "exampleText1234"

-- | Reject if invalid char
exampleInvalidChar :: Either FixedTextErrors (FixedText 30 1 "[[:digit:]]")
exampleInvalidChar = fixedTextFromString "exampleNotAllDigits"
