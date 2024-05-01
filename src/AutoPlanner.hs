module AutoPlanner where

import Units
import Traits
import SetData
import UnitLister
import ActiveTraitList

import Data.List
import Data.Ord


autoPlanner :: Board -> SetData -> Int -> Int -> Int -> Board
autoPlanner currentBoard set limit1 limit2 limit3 = do
    let board = autoPlanner1 currentBoard set limit1 limit2
    maximumBy (comparing (scoreBoard set)) (allBoards board)
    where
        allBoards board = buildBoards board set limit3


autoPlanner1 :: Board -> SetData -> Int -> Int -> Board
autoPlanner1 currentBoard set limit1 limit2 = do
    let board = autoPlanner2 currentBoard set limit1
    maximumBy (comparing (scoreBoard set)) (allBoards board)
    where
        allBoards board = buildBoards board set limit2

autoPlanner2 :: Board -> SetData -> Int -> Board
autoPlanner2 currentBoard set limit = bestBoard
    where
        allBoards = buildBoards currentBoard set limit
        bestBoard = maximumBy (comparing (scoreBoard set)) allBoards

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


scoreBoard :: SetData -> Board -> Int
scoreBoard set board = sum (map cost board) + 4 * sum activeTiers - length activeTiers
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