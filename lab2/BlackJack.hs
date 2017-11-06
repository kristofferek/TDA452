module BlackJack where
import Cards
import RunGame

-- Returns an empty hand
empty :: Hand
empty = Empty

--
value :: Hand -> Integer
value Empty = 0
value (Add c h) = rank c + value h

card1 = Card Numeric 3 Spades
card2 = Card Numeric 5 Hearts

hand = Add card1 Empty
