module BlackJack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import System.Random

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


--                      PART A - Functions


-- Returns an empty hand
empty :: Hand
empty = Empty

-- Returns the total amount of Aces in a hand
nbrOfAces :: Hand -> Integer
nbrOfAces Empty = 0
nbrOfAces (Add (Card Ace _) h) = 1 + nbrOfAces h
nbrOfAces (Add c h)            = nbrOfAces h

-- Returns the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric i)  = i
valueRank Ace          = 11
valueRank _            = 10

-- Returns the total value of a hand
value :: Hand -> Integer
value h
  | value' h <= 21 = value' h
  | otherwise      = value' h - 10 * nbrOfAces h
  where
    value' Empty = 0
    value' (Add (Card r _) h)  = valueRank r + value' h

-- Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver = (> 21) . value

-- Which player is the winner? Bank or Guest?
-- Guest is the first parameter
winner g b
  | gameOver g               = Bank
  | gameOver b               = Guest
  | value g > value b        = Guest
  | otherwise                = Bank




--                  PART B

(<+) :: Hand -> Hand -> Hand
(<+) (Add c1 Empty) h2 = (Add c1 h2)
(<+) (Add c1 h1) h2 = (Add c1 (h1<+h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size (h1<+h2)) == (size h1 + size h2)

fullDeck :: Hand
fullDeck = (suitDeck Hearts)<+(suitDeck Spades)
              <+(suitDeck Diamonds)<+(suitDeck Clubs)

suitDeck :: Suit -> Hand
suitDeck s = suitDeck' s 2
  where
    suitDeck' s i
      | i <= 10   = (Add (Card (Numeric i) s) (suitDeck' s (i+1)))
      | i == 11   = (Add (Card Jack s) (suitDeck' s (i+1)))
      | i == 12   = (Add (Card Queen s) (suitDeck' s (i+1)))
      | i == 13   = (Add (Card King s) (suitDeck' s (i+1)))
      | otherwise = (Add (Card Ace s) Empty)


--draw :: Hand -> Hand -> (Hand,Hand)
--error "draw: The deck is empty."

--first :: (a, b) -> a
--first (x,y) = x

--playBank :: Hand -> Hand
--shuffle :: StdGen -> Hand -> Hand



-- Example hands
card1 = Card (Numeric 3) Spades
card2 = Card Ace Hearts
card3 = Card Queen Clubs
hand1 = Add card2 (Add card2 (Add card1 Empty)) -- Should have value 5
hand2 = Add card3 (Add card1 (Add card1 Empty)) -- Should have value 16
