module Loop where

import LoadSet
import SetData
import Traits
import Units
import UnitLister

import Data.List.Split
import Data.Maybe
--import Brick

board :: [Unit]
board = []

activeTraits :: [Trait]
activeTraits = []

setSelection :: IO (SetData)
setSelection = do
    putStrLn "Choose a set: 10"
    i <- getLine
    case i of
        "10" -> loadSet "src/Set10.txt"
        _ -> setSelection


loop :: IO ()
loop = do

    set <- setSelection
    putStrLn "Loaded"

    loop set board activeTraits
    where   loop :: SetData -> [Unit] -> [Trait] -> IO ()
            loop set board activeTraits = do
                                    putStrLn (show board)
                                    putStrLn (show activeTraits)

                                    i <- getLine
                                    let x = splitOn " " i
                                    case head x of
                                        "add" -> if isJust (getUnit (last x) set) then loop set (board ++ [fromJust (getUnit (last x) set)]) activeTraits else loop set board activeTraits 
                                        "close" -> putStr "Closed program \n"
                                        "help" -> do
                                            putStrLn "add <unit>: to add a unit to the board \nrem <unit>: to remove a unit from the board\nlist <cost/trait>: lists all the units of that cost/trait\nclose: to close the program\n"
                                            loop set board activeTraits
                                        "" -> loop set board activeTraits
                                        _ -> loop set board activeTraits

                                    --loop set board activeTraits

