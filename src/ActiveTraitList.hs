module ActiveTraitList where
import Traits
import Units
import SetData
import Data.Maybe

import Data.List 

data ActiveTrait = ActiveTrait {
    traitname :: TraitName
    , ctier :: Int
    , ntier :: Int
    }

instance Show ActiveTrait where
    show aTrait = "\n" ++ traitname aTrait ++ ": " ++ show (ctier aTrait) ++ "/" ++ show (ntier aTrait)

type ActiveTraitList = [ActiveTrait]
type Board = [Unit]

newActiveTraitList :: Board -> SetData -> ActiveTraitList
newActiveTraitList board set = map construct counts
    where
        allTraits = sort $ concatMap Units.traits board 
        groupedTraits = group allTraits
        counts = [(head x, length x) | x <- groupedTraits]
        construct (trait, count) = ActiveTrait {traitname = trait, ctier = count, ntier = hActiveTier (tiers (fromJust (getTrait trait set))) count}
        hActiveTier (t:tiers) x 
            | [] == tiers = t
            | t <= x = hActiveTier tiers x
            | t > x = t
            | otherwise = 0