module AutoPlanner where

import Units
import Traits
import SetData
import UnitLister
import ActiveTraitList

import Data.List ( maximumBy )
import Data.Ord ( comparing )

autoPlanner :: Board -> SetData -> Int -> Int -> Int -> Board
autoPlanner currentBoard set start inc max
    | start > max = currentBoard
    | otherwise = autoPlanner (maximumBy (comparing (scoreBoard set)) (allBoards currentBoard)) set (start+inc) inc max
    where
        allBoards board = buildBoards board set start

potentialUnits :: Board -> SetData -> [Unit]
potentialUnits board set = [x | x <- concatMap ((unitLister (units set) . Left) . traitname) atl, x `notElem` board]
    where
        atl = newActiveTraitList board set

buildBoards :: Board -> SetData -> Int -> [Board]
buildBoards board set maxSize
    | length board >= maxSize = [board]
    | otherwise = do
        let potentials = potentialUnits board set
        unit <- potentials
        buildBoards (unit : board) set maxSize

-- Current scoring priotizing higher tiers of traits.
scoreBoard :: SetData -> Board -> Int
scoreBoard set board = sum (map cost board) + sum (map (^2) activeTiers)
    where
        activeTiers = map activeTier (newActiveTraitList board set)

{-
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
-}