module ActiveTraitList where
import Traits
import Units
import SetData
import Data.Maybe

import Data.List 

data ActiveTrait = ActiveTrait {
    traitname :: TraitName
    , activeTier :: Int
    , currentTier :: Int
    , nextTier :: Int
    }

instance Show ActiveTrait where
    show aTrait = traitname aTrait ++ " " ++ show (activeTier aTrait) ++ ": " ++ show (currentTier aTrait) ++ "/" ++ show (nextTier aTrait) ++ "\n"

type ActiveTraitList = [ActiveTrait]
type Board = [Unit]

newActiveTraitList :: Board -> SetData -> ActiveTraitList
newActiveTraitList board set = map construct counts
    where
        allTraits = sort $ concatMap Units.traits board 
        groupedTraits = group allTraits
        counts = [(head x, length x) | x <- groupedTraits]
        construct (trait, count) = ActiveTrait {traitname = trait, activeTier = lActiveTier (tiers (fromJust (getTrait trait set))) count, currentTier = count, nextTier = hActiveTier (tiers (fromJust (getTrait trait set))) count}
        lActiveTier (t:tiers) x
            | [] == tiers = t
            | t > x = 0
            | head tiers <= x = lActiveTier tiers x
            | head tiers > x = t
            | otherwise = 0

        hActiveTier (t:tiers) x 
            | [] == tiers = t
            | t <= x = hActiveTier tiers x
            | t > x = t
            | otherwise = 0


toNextTier :: ActiveTrait -> Int
toNextTier ActiveTrait{currentTier = c, nextTier = n} = n - c