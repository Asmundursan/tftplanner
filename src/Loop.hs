module Loop where

import LoadSet
import SetData
import Traits
import Units
import UnitLister
import ActiveTraitList
import AutoPlanner

import Data.List.Split
import Data.Maybe
import Text.Read
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

addToBoard :: UnitName -> SetData -> Board -> Board
addToBoard unitName set board
    | isJust (getUnit unitName set) = board ++ [fromJust (getUnit unitName set)]
    | otherwise = board

list :: String -> SetData -> String
list str set
    | str == "units" = show (units set)
    | str == "traits" = show (SetData.traits set)
    | isJust (readMaybe str :: Maybe Integer) = show (unitLister (units set) (Right (read str)))
    | otherwise = show (unitLister (units set) (Left str))

helpText :: String 
helpText = "add <unit>: to add a unit to the board \nrm <unit>: to remove a unit from the board\nnew: to make a new board\nlist <cost/trait>: lists all the units of that cost/trait\nlist units: lists ALL units\nlist traits: list all traits and their tiers\nhelp: show this text\nclose: closes the program\n"

initialize :: IO ()
initialize = do
    set <- setSelection
    putStrLn "Loaded"
    putStrLn helpText
    loop set newBoard 

loop :: SetData -> Board -> IO ()
loop set board = do
    putStrLn (show board)
    putStrLn (show (newActiveTraitList board set))

    i <- getLine
    let x = splitOn " " i
    case head x of
        "add" -> loop set (addToBoard (last x) set board)
        "rm" -> loop set (filter (/= fromJust(getUnit (last x) set)) board)
        "new" -> loop set newBoard
        "close" -> putStr "Closed program \n"
        "list" -> do
            putStrLn (list (last x) set)
            loop set board
        "help" -> do
            putStrLn helpText
            loop set board
        "auto" -> loop set (autoPlanner board set 10)
        "" -> loop set board
        _ -> loop set board