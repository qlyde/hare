{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
module Untyped where
import Control.Applicative 
import HareMonad (Hare, hare, failure, readCharacter)
import Control.Monad(guard)

data RE = Empty -- Matches the empty string
        | Fail -- Matches no strings
        | Char [Char] -- Matches a single character from the list
        | Seq RE RE -- Matches the two expressions in sequence
        | Choose RE RE -- Matches either the first, or the second
        | Star RE -- Matches the expression zero or more times

data Results = None
             | Character Char 
             | Tuple Results Results 
             | Repetition [Results]
             deriving (Show)
             

match :: (Monad f, Alternative f) => RE -> Hare f Results
match Empty = pure None
match Fail = failure
match (Char cs) = do 
    x <- readCharacter
    guard (x `elem` cs)
    pure (Character x)
match (Seq a b) = do 
    ra <- match a 
    rb <- match b  
    pure (Tuple ra rb)
match (Choose a b) = 
        match a
    <|> match b
match (Star a) =
        addFront <$> match a <*> match (Star a)
    <|> pure (Repetition [])
  where 
    addFront x (Repetition xs) = Repetition (x:xs)
    addFront _ _ = error "(should be) impossible!"

(=~) :: (Monad f, Alternative f) => String -> RE -> f Results
str =~ re = hare matchAnywhere str
  where matchAnywhere = match re <|> (readCharacter >> matchAnywhere)