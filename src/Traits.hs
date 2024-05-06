module Traits where

data Trait = Trait {
    name :: TraitName
    , tiers ::  TraitTiers
    }
    deriving (Read, Eq)

instance Show Trait where
    show trait = "\n" ++ name trait ++ " " ++ show (tiers trait) 

type TraitName = String
type TraitTiers = [Int]

newTrait :: String -> Trait
newTrait text = Trait{name = getName text, tiers = getNums text}
    where
        getName = head . words
        getNums = map read . tail . words 