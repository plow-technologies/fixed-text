{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Module      : Example.Properties.Types.FixedText
Description : Text type with constraints on allowable input and length
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

FixedText is designed to format incoming text strings with a set of valid
characters that are known at compile time.

| -}


module Data.FixedText  where



import Data.Text (pack,Text)
import qualified Data.Text as Text
import Data.Proxy (Proxy(..))

import GHC.TypeLits (Nat,natVal,KnownNat,Symbol,KnownSymbol,symbolVal)
import Data.Monoid
import Text.Regex.Lens
import Text.Regex.Base
import Text.Regex.Posix
import Control.Lens
import Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as QuickCheck
import qualified Regex.Genex as Genex



newtype RegexString = RegexString{ unRegex :: String}
  deriving (Show,Eq,Ord)

-- | Set of things that can go wrong with Fixed Text construction
data FixedTextErrors = FixedTextErrorMin
                     | FixedTextErrorRegex RegexString String
                     | FixedTextErrorMax
  deriving (Show,Eq,Ord)


-- | Text array with max size and min size and character set
newtype  FixedText (lengthMax :: Nat)
                   (lengthMin :: Nat)
                   (regex     :: Symbol) 
           = FixedText { _unFixedText :: Text}
  deriving (Show,Ord,Eq)



-- | String version of the Smart Constructor
fixedTextFromString :: forall max min regex . 
  ( KnownNat    max
  , KnownNat    min
  , KnownSymbol regex) => String -> Either FixedTextErrors (FixedText max min regex)
fixedTextFromString str = final
  where
    max'          = fromIntegral $ natVal (Proxy :: Proxy max)
    min'          = fromIntegral $ natVal (Proxy :: Proxy min)    
    isTooLittle   = length str < min'
    regexStr      = RegexString $ symbolVal (Proxy :: Proxy regex)
    trimmedString = take max' str
    notRegex      = notValidRegex (unRegex regexStr) trimmedString
    final
      | isTooLittle = Left   FixedTextErrorMin
      | notRegex    = Left (FixedTextErrorRegex regexStr trimmedString)
      | otherwise   = (Right . FixedText .   pack) trimmedString  


-- | Text version of the Smart Constructor
fixedTextFromText :: forall max min regex . 
  ( KnownNat    max
  , KnownNat    min
  , KnownSymbol regex) => Text -> Either FixedTextErrors (FixedText max min regex)
fixedTextFromText txt = final
  where
    max'          = fromIntegral $ natVal (Proxy :: Proxy max)
    min'          = fromIntegral $ natVal (Proxy :: Proxy min)    
    isTooLittle   = Text.length txt < min'
    regexStr      = RegexString $ symbolVal (Proxy :: Proxy regex)
    trimmedText   = Text.take max' txt
    notRegex      = notValidRegex (unRegex regexStr) (Text.unpack trimmedText)
    final
      | isTooLittle = Left   FixedTextErrorMin
      | notRegex    = Left (FixedTextErrorRegex regexStr (Text.unpack trimmedText))
      | otherwise   = (Right . FixedText  ) trimmedText  

notValidRegex :: String -> String -> Bool
notValidRegex regexStr txt =  regexPart /= txt
  where
    regexPart     = txt ^. regex compiledRegex . matchedString
    compiledRegex :: Regex
    compiledRegex = makeRegex regexStr



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


-- | Instances to define


-- | Monoid instance with 0 min
-- No FixedText besides one that has a minimum size of zero
-- should be a Monoid.
instance ( KnownNat max
         , KnownSymbol regex) => 
 Monoid (FixedText (max::Nat) (0::Nat) (regex::Symbol)) where
  mempty                      = FixedText ""
  mappend s1@(FixedText str1)
             (FixedText str2) = either (const s1)
                                        id
                                        (fixedTextFromText (str1 <> str2))






-- | Arbitrary instance
-- This arbitrary instance takes advantage of the Monoid defined above
instance ( KnownNat     max
         , KnownSymbol  regex) => 
  Arbitrary (FixedText max 0 regex) where

    arbitrary = let regexStr        = symbolVal (Proxy :: Proxy regex)        
                    generatedString = Genex.genexPure [regexStr]

                 in either (const mempty) id <$>
                            QuickCheck.elements
                              (fixedTextFromString <$>
                                         generatedString)




