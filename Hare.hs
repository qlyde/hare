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

infixr `cons`
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action (\(a, as) -> a : as) $ Seq x xs -- type of `Seq x xs` is `RE (a, [a])`
                                                   -- Action function must have type `(a, [a]) -> [a]`

string :: String -> RE String
string xs = cons 

rpt :: Int -> RE a -> RE [a]
rpt n re = error "'rpt' unimplemented"

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re = error "'rptRange' unimplemented"

option :: RE a -> RE (Maybe a)
option re = error "'option' unimplemented"

plus :: RE a -> RE [a]
plus re = cons re $ Star re

choose :: [RE a] -> RE a
choose res = error "'choose' unimplemented"

