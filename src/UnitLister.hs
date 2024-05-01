module UnitLister where
import Traits
import Units
import Data.Char (toLower)

unitLister :: [Unit] -> Either TraitName Int -> [Unit]
unitLister unitList (Left trait) = filter (traitFinder trait) unitList
    where traitFinder trait unit = map toLower trait `elem` map (map toLower) (Units.traits unit)
unitLister unitList (Right cost)  = filter (costFinder cost) unitList
    where costFinder cost unit = cost == Units.cost unit