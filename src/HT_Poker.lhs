================================================================================

                            Yet Another Haskell Tutorial
                                    Hal Daume III

================================================================================
Chapter 6
Modules

In Haskell, program subcomponents are divided into modules. Each module sits in its
own file and the name of the module should match the name of the file (without the
".hs" extension, of course), if you wish to ever use that module in a larger program.
  For instance, suppose I am writing a game of poker. I may wish to have a separate
module called "Cards" to handle the generation of cards, the shuffling and the dealing
functions, and then use this "Cards" module in my "Poker" modules. That way, if I
ever go back and want to write a blackjack program, I don't have to rewrite all the code
for the cards; I can simply import the old "Cards" module.

6.1 Exports

Suppose as suggested we are writing a cards module. I have left out the implementation
details, but suppose the skeleton of our module looks something like this:

> module Cards
>     where

> data Card = ...
> data Deck = ...

> newDeck :: ... -> Deck
> newDeck = ...

> shuffle :: ... -> Deck -> Deck
> shuffle = ...

> -- 'deal deck n' deals 'n' cards from 'deck'
> deal :: Deck -> Int -> [Card]
> deal deck n = dealHelper deck n []
> dealHelper = ...

In this code, the function deal calls a helper function dealHelper. The implementation
of this helper function is very dependent on the exact data structures you
used for Card and Deck so we don't want other people to be able to call this function.
In order to do this, we create an export list, which we insert just after the module name
declaration:

> module Cards ( Card(),
>                Deck(),
>                newDeck,
>                shuffle,
>                deal
>              )
>     where
>     ...

Here, we have specified exactly what functions the module exports, so people who
use this module won't be able to access our dealHelper function. The () after
Card and Deck specify that we are exporting the type but none of the constructors.
For instance if our definition of Card were:

> data Card = Card Suit Face
> data Suit = Hearts
>           | Spades
>           | Diamonds
>           | Clubs
> data Face = Jack
>           | Queen
>           | King
>           | Ace
>           | Number Int

Then users of our module would be able to use things of type Card, but wouldn't
be able to construct their own Cards and wouldn't be able to extract any of the suit/face
information stored in them.
  If we wanted users of our module to be able to access all of this information, we
would have to specify it in the export list:

> module Cards ( Card(Card),
>                Suit(Hearts,Spades,Diamonds,Clubs),
>                Face(Jack,Queen,King,Ace,Number),
>                ...
>              )
>   where
>   ...

This can get frustrating if you're exporting datatypes with many constructors, so if
you want to export them all, you can simply write (..), as in:

> module Cards ( Card(..),
>                Suit(..),
>                Face(..),
>                ...
>              )
>     where
>     ...

And this will automatically export all the constructors.

6.2 Imports

There are a few idiosyncracies in the module import system, but as long as you stay
away from the corner cases, you should be fine. Suppose, as before, you wrote a
module called "Cards" which you saved in the file "Cards.hs". You are now writing
your poker module and you want to import all the definitions from the "Cards" module.
To do this, all you need to do is write:

> module Poker
>     where
> import Cards

This will enable to you use any of the functions, types and constructors exported
by the module "Cards". You may refer to them simply by their name in the "Cards"
module (as, for instance, newDeck), or you may refer to them explicitely as imported
from "Cards" (as, for instance, Cards.newDeck). It may be the case that two module
export functions or types of the same name. In these cases, you can import one of
the modules qualified which means that you would no longer be able to simply use
the newDeck format but must use the longer Cards.newDeck format, to remove
ambiguity. If you wanted to import "Cards" in this qualified form, you would write:

> import qualified Cards

Another way to avoid problems with overlapping function definitions is to import
only certain functions from modules. Suppose we knew the only function from "Cards"
that we wanted was newDeck, we could import only this function by writing:

> import Cards (newDeck)

On the other hand, suppose we knew that that the deal function overlapped with
another module, but that we didn't need the "Cards" version of that function. We could
hide the definition of deal and import everything else by writing:

> import Cards hiding (deal)

Finally, suppose we want to import "Cards" as a qualified module, but don't want
to have to type Cards. out all the time and would rather just type, for instance, C. ?
we could do this using the as keyword:

> import qualified Cards as C

These options can be mixed and matched ? you can give explicit import lists on
qualified/as imports, for instance.

================================================================================