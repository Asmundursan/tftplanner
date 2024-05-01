module AutoPlanner where

import Units
import Traits
import SetData
import UnitLister
import ActiveTraitList

import Data.List
import Data.Ord



autoPlanner :: Board -> SetData -> Int -> Board
autoPlanner board set limit
   | length board >= limit = board
   | otherwise = autoPlanner nextBoard set limit
        where
            atl = newActiveTraitList board set
            potensialUnits = [x | x <- concatMap ((unitLister (units set) . Left) . traitname) atl, x `notElem` board]
            unitScore = [(unit, scoreUnit unit atl) | unit <- potensialUnits]
            highestScoringUnit = fst $ maximumBy (comparing snd) unitScore
            nextBoard = board ++ [highestScoringUnit]

scoreUnit :: Unit -> ActiveTraitList -> Int
scoreUnit unit atl = cost unit + 2 * sum (map fst allTiers) + 3 * tierBreak + 2 * length allTiers
    where
        allTiers = [(ctier j, toNextTier j) | i <- Units.traits unit, j <- atl, i == traitname j && toNextTier j /= 0]
        tierBreak = sum [snd i | i <- allTiers , snd i == 1] + 1