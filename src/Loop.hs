module Loop where

import LoadSet ( loadSet )
import SetData ( SetData(units, traits), getUnit )
import Units ( UnitName )
import UnitLister ( unitLister )
import ActiveTraitList ( newActiveTraitList, Board )
import AutoPlanner ( autoPlanner )

import Data.Maybe ( fromJust, isJust )
import Text.Read ( readMaybe )

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

helpText :: IO ()
helpText = putStr "new: clears the board\nadd <unit>: to add a unit to the board \nrm <unit>: to remove a unit from the board\nnew: to make a new board\nlist <cost/trait>: lists all the units of that cost/trait\nlist units: lists ALL units\nlist traits: list all traits and their tiers\nauto: makes you a board of 10 units that fits with the units you have on the board\nchange: goes back to the set selection\nclose: closes the program\n"

clear :: IO ()
clear = putStr "\ESC[2J"

prettyPrint :: String -> IO ()
prettyPrint = putStrLn . filter (not . (`elem` ",[]"))

initialize :: IO ()
initialize = do
    clear
    set <- setSelection
    clear
    putStrLn "Loaded"
    helpText
    loop set newBoard ""

loop :: SetData -> Board -> String -> IO ()
loop set board msg = do

    clear
    helpText
    putStrLn ""
    prettyPrint (show board)
    prettyPrint (show (newActiveTraitList board set))
    if null msg then putStr "" else prettyPrint msg

    i <- getLine
    let x = words i
    case head x of
        "add" -> loop set (addToBoard (last x) set board) ""
        "rm" -> loop set (filter (/= fromJust(getUnit (last x) set)) board) ""
        "new" -> loop set newBoard ""
        "close" -> putStr "Closed program \n"
        "list" -> loop set board (list (last x) set)
        "auto" -> if null board then loop set board "Please add at least 1 unit to the board before using the autoplanner" else loop set (autoPlanner board set 4 3 10) ""
        "change" -> initialize
        "" -> loop set board ""
        _ -> loop set board ""