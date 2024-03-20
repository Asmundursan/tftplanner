module UnitLister where
import Traits
import Units

unitLister :: Either TraitName Int -> [Unit] -> (Either TraitName Int,[Unit])
unitLister (Left trait) unitList = (Left trait, filter (traitFinder trait) unitList)
    where traitFinder trait unit = trait `elem` Units.traits unit
unitLister (Right cost) unitList = (Right cost, filter (costFinder cost) unitList)
    where costFinder cost unit = cost == Units.cost unit