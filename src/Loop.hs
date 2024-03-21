module Loop where

import LoadSet
import SetData
import Traits
import Units
import UnitLister

import Data.List.Split
import Data.Maybe
--import Brick

type Board = [Unit]
type ActiveTraitList = [Trait]

newBoard :: Board
newBoard = []

newActiveTraits :: ActiveTraitList
newActiveTraits = []

setSelection :: IO SetData
setSelection = do
    putStrLn "Choose a set: 10 or 11"
    i <- getLine
    case i of
        "10" -> loadSet "src/Set10.txt"
        "11" -> loadSet "src/Set11.txt"
        _ -> setSelection
{-
traitUpdate :: TraitList -> ActiveTraitList -> ActiveTraitList
traitUpdate (x:xs) (y:ys) 
    | x == head y = 
-}

addToBoard :: UnitName -> SetData -> ActiveTraitList -> Board -> Board
addToBoard unitName set traits board
    | isJust (getUnit unitName set) = board ++ [fromJust (getUnit unitName set)]
    | otherwise = board

initialize :: IO ()
initialize = do
    set <- setSelection
    putStrLn "Loaded"
    loop set newBoard newActiveTraits

loop :: SetData -> Board -> ActiveTraitList -> IO ()
loop set board activeTraits = do
    putStrLn (show board)
    putStrLn (show activeTraits)

    i <- getLine
    let x = splitOn " " i
    case head x of
        "add" -> loop set (addToBoard (last x) set activeTraits board) activeTraits
        "close" -> putStr "Closed program \n"
        "list" -> do
            putStrLn (show set)
            loop set board activeTraits
        "help" -> do
            putStrLn "add <unit>: to add a unit to the board \nrem <unit>: to remove a unit from the board\nlist <cost/trait>: lists all the units of that cost/trait\nclose: to close the program\n"
            loop set board activeTraits
        "" -> loop set board activeTraits
        _ -> loop set board activeTraits

