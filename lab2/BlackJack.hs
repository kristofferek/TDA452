module BlackJack where
import Cards
import RunGame

{-                    TASK 3.1

  We have read the document carefully
-}


{-                    TASK 3.2

size hand2
  = size (Add (Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2

Step 1) size will match the above statement with the
        following pattern "(Add card hand)",
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


--                      TASK 3.4


-- Returns an empty hand
empty :: Hand
empty = Empty

-- Returns the total amount of Aces in a hand
nbrOfAces :: Hand -> Integer
nbrOfAces Empty = 0
nbrOfAces (Add (Card r _) h) | r == Ace = 1 + nbrOfAces h
nbrOfAces (Add c h)                     = nbrOfAces h

-- Returns the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric i)           = i
valueRank r | r == Ace          = 11
valueRank _                     = 10

-- Returns the total value of a hand
value :: Hand -> Integer
value h = if value' h <= 21 && nbrOfAces h > 0
  then value' h
  else value' h - 10*nbrOfAces h --Reduces the value of ace
  where
    value' Empty = 0
    value' (Add (Card r _) h)  = valueRank r + value' h

-- Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- Which player is the winner? Bank or Guest?
-- Guest is the first parameter
winner :: Hand -> Hand -> Player
winner g b = if value g > value b
  then Guest
  else Bank

-- Example hands
card1 = Card (Numeric 3) Spades
card2 = Card Ace Hearts
card3 = Card Queen Clubs
hand1 = Add card2 (Add card2 (Add card1 Empty)) -- Should have value 5
hand2 = Add card3 (Add card1 (Add card3 Empty)) -- Should have value 23
