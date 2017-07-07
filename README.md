# fixed-text
Create Text fields that have max and min bounds, as well as character classes.

Fixed Text looks like...
``` haskell
newtype  FixedText (lengthMax :: Nat)
                   (lengthMin :: Nat)
                   (regex     :: Symbol) 
           = FixedText { _unFixedText :: Text}
  deriving (Ord,Eq)
```

The [Regex](https://en.wikibooks.org/wiki/Regular_Expressions/POSIX_Basic_Regular_Expressions) portion expects a 
character class in posix regex syntax.

<table>
<tr><th>POSIX class</th> 	<th>similar to</th> 	<th> meaning</th></tr>
<tr><td>[:upper:] </td>     <td>   	[A-Z] </td> 	  <td> uppercase letters</td>
<tr><td>[:lower:] </td>     <td>   	[a-z] </td>	      <td> lowercase letters</td>
<tr><td>[:alpha:] </td>     <td>   	[A-Za-z] </td>	  <td> upper- and lowercase letters</td>
<tr><td>[:digit:] </td>     <td>   	[0-9] </td>	      <td> digits</td>
<tr><td>[:xdigit:]</td>     <td>    [0-9A-Fa-f]</td>  <td> hexadecimal digits</td>
<tr><td>[:alnum:] </td>     <td>   	[A-Za-z0-9]</td>  <td> digits, upper- and lowercase letters</td>
<tr><td>[:punct:] </td>                               <td> punctuation (all graphic characters except letters and digits)</td>
<tr><td>[:blank:] </td>     <td>   	[ \t] </td>	      <td> space and TAB characters only</td>
<tr><td>[:space:] </td>     <td>   	[ \t\n\r\f\v]</td><td> blank (whitespace) characters</td>
<tr><td>[:cntrl:] </td>     <td>   	             	  <td> control characters</td>
<tr><td>[:graph:] </td>     <td>   	[^ [:cntrl:]] 	</td> <td> graphic characters (all characters which have graphic representation)</td>
<tr><td>[:print:] </td>     <td>   	[[:graph] ] </td> <td> graphic characters and space</td>
</table>
## Usage




A few examples
``` haskell
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
```

```
λ> exampleFixedText
Right exampleText1234
```

```
λ> :set -XOverloadedStrings
λ> "exampleText1234" :: StandardFixedText
exampleText1234
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
