module Loop where

import LoadSet
import SetData
import Traits
import Units
import UnitLister
import ActiveTraitList

import Data.List.Split
import Data.Maybe
--import Brick



newBoard :: Board
newBoard = []

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

addToBoard :: UnitName -> SetData -> Board -> Board
addToBoard unitName set board
    | isJust (getUnit unitName set) = board ++ [fromJust (getUnit unitName set)]
    | otherwise = board

initialize :: IO ()
initialize = do
    set <- setSelection
    putStrLn "Loaded"
    loop set newBoard 

loop :: SetData -> Board -> IO ()
loop set board = do
    putStrLn (show board)
    putStrLn (show (newActiveTraitList board set))

    i <- getLine
    let x = splitOn " " i
    case head x of
        "add" -> loop set (addToBoard (last x) set board)
        "rem" -> loop set (filter (/= fromJust(getUnit (last x) set)) board)
        "close" -> putStr "Closed program \n"
        "list" -> do
            putStrLn (show set)
            loop set board
        "help" -> do
            putStrLn "add <unit>: to add a unit to the board \nrem <unit>: to remove a unit from the board\nlist <cost/trait>: lists all the units of that cost/trait\nclose: to close the program\n"
            loop set board
        "" -> loop set board
        _ -> loop set board

