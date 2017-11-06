module BlackJack where
import Cards
import RunGame

{- Task 3.1
  We have read the document carefully
-}

{- Task 3.2
size (Add (Card (Numeric 2) Hearts)
        (Add (Card Jack Spades) Empty))

Step 1) size will match the hand with the followin pattern "(Add card hand)",
        where card in this case = "(Card (Numeric 2) Hearts))"
        and hand = (Add (Card Jack Spades) Empty)

Step 2) size will, as of now, return "1 + size hand"
        hand is not empty so pattern "(Add card hand)" will match.
        card = (Card Jack Spades)
        hand = Empty

Step 3) Now the return will be "1 + 1 + size hand".
        And since hand = Empty, 0 will be returned.

Lastly, 1+1+0=2 will be returned from the initial call
-}

-- Returns an empty hand
empty :: Hand
empty = Empty

-- Returns the total value of a hand
value :: Hand -> Integer
value Empty = 0
value (Add (Card (Numeric i) _) h) = i + value h
value (Add (Card r _) h) | r == Ace = 11 + value h
value (Add _ h) = 10 + value h

-- Example hand
card1 = Card (Numeric 3) Spades
card2 = Card Ace Hearts
hand = Add card2 (Add card1 (Add card2 Empty))
