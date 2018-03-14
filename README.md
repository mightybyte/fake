Fake is Haskell package for generating realistic-looking fake data.  The
package has three main components:

1. An analog to QuickCheck's
   [Arbitrary](http://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Arbitrary.html#t:Arbitrary)
   and
   [Gen](http://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Gen.html#t:Gen)
   that use realistic probability distributions rather than the more uniform
   distributions used by QuickCheck.
1. A [generic coverage function](src/Fake/Cover.hs#L114)
   that generates [full constructor coverage](test/Main.hs#L32)
   over a data type.
1. A [suite of providers](src/Fake/Provider)
   for common types of data such as names, addresses, phone numbers, ID
   numbers, etc.

Fake's `gcover` function is particularly useful with the [armor
package](https://github.com/mightybyte/armor) for ensuring that all
constructors of your data types are tested for backwards compatible
serializations without having to write all the values yourself.  This allows
you to get higher confidence that you have covered most of the important cases
without the combinatorial explosion of exhaustive testing.

## Credits

Original inspiration came from the production needs of
[Formation](http://formation.ai/) (previously Takt).

Providers and other details inspired by similar packages in
[Python](https://github.com/joke2k/faker) and
[Ruby](https://github.com/stympy/faker).  
