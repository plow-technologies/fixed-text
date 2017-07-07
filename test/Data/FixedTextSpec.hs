
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
import Data.Either

import Data.Proxy (Proxy(..))

import GHC.TypeLits (Nat,natVal,KnownNat,Symbol,KnownSymbol,symbolVal)
import qualified Regex.Genex as Genex


tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Test: exampleText1234"          ((fixedTextToString <$>  exampleFixedText) `compare` (Right "exampleText1234") @?= EQ)
  , testCase "Test: exampleText1234 Literal"  ((fixedTextToString     exampleFixedText') `compare`         "exampleText1234" @?= EQ)

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

exampleFixedText'  :: (FixedText 30 0 "[[:alnum:]]")
exampleFixedText' = "exampleText1234" 

-- | Cut off too much input.
exampleOverFlowProtection :: Either FixedTextErrors (FixedText 10 1 "[[:alnum:]]")
exampleOverFlowProtection = fixedTextFromString "exampleText1234" 

-- | Reject if below min input
exampleUnderFlowProtection :: Either FixedTextErrors (FixedText 200 20 "[[:alnum:]]")
exampleUnderFlowProtection = fixedTextFromString "exampleText1234"

-- | Reject if invalid char
exampleInvalidChar :: Either FixedTextErrors (FixedText 30 1 "[[:digit:]]")
exampleInvalidChar = fixedTextFromString "exampleNotAllDigits"


type StandardFixedText = (FixedText 140 0 "[[:alnum:]]")


