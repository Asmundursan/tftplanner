module Loop where

import LoadSet
import SetData
import Traits
import Units
import UnitLister

board :: [Unit]
board = []

activeTraits :: [Trait]
activeTraits = []

loop :: IO ()
loop = do
    set <- loadSet "src/Set10.txt"
    putStrLn (show (unitLister (Left "Edgelord") (units set)))

    loop set board activeTraits
    where   loop :: SetData -> [Unit] -> [Trait] -> IO ()
            loop set board activeTraits = do
                                    putStrLn (show board)
                                    putStrLn (show activeTraits)
{-
                                    i <- getLine
                                    case i of
                                        i -> board : (getUnit i set)
                                        "q" -> putStr "Closed program \n"
                                        "" -> loop set board activeTraits
                                        _ -> loop set board activeTraits

                                    --loop set board activeTraits
                                    -}