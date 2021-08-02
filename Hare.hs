{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a, b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty          = pure ()
match Fail           = failure
match (Char cs)      = do
  c <- readCharacter
  guard (c `elem` cs)
  pure c
match (Seq    ra rb) = (,) <$> match ra <*> match rb
match (Choose ra rb) = match ra <|> match rb
match (Star   ra)    = ((:) <$> match ra <*> match (Star ra)) <|> pure []
match (Action f  ra) = f <$> match ra

matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

-- The cons function matches a regular expression for an element of type a, followed by a
-- regular expression for a list [a], and returns that element prepended to the list.
infixr `cons`
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action (\(a, as) -> a : as) $ Seq x xs -- type of `Seq x xs` is `RE (a, [a])`
                                                   -- Action function must have type `(a, [a]) -> [a]`

-- The string function produces a regex that only matches the given string, and returns it.
string :: String -> RE String
string []       = Action (\_ -> []) Empty -- Empty matches empty string
string (x : xs) = cons (Char [x]) (string xs)

-- The rpt combinator allows a regular expression to be repeated a fixed number of times.
-- The expression rpt n x is equivalent to repeating x in sequence n times, returning the
-- results in a list.
rpt :: Int -> RE a -> RE [a]
rpt n re
  | n <= 0    = Action (\_ -> []) Empty
  | otherwise = cons re $ rpt (n - 1) re

-- Lastly, the rptRange function takes a range of numbers (x, y). You may assume that
-- x <= y. It will match the given regex at least x times and at most y times.
rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x, y) re
  | x == y    = rpt x re
  | otherwise = Choose (rpt y re) $ rptRange (x, y - 1) re

-- The option function matches the given expression zero or one times. In other words,
-- if the expression matches, it returns Just that match, otherwise Nothing
option :: RE a -> RE (Maybe a)
option re = Choose (Action Just re) (Action (\_ -> Nothing) Empty)

-- The plus function is like Star, but requires the expression given to occur at least once.
plus :: RE a -> RE [a]
plus re = cons re $ Star re

-- The choose function is like the Choose constructor, but generalised to lists. That is,
-- choose [a, b] is equivalent to a ‘Choose‘ b. What should choose [] be?
choose :: [RE a] -> RE a
choose []         = Fail
choose (re : res) = Choose re $ choose res

