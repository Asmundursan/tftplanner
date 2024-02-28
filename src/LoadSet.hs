module LoadSet where
import SetData
import Traits
import Units

loadSet :: String -> IO SetData
loadSet set = do
    contents <- readFile set
    --print (readLines (lines contents) T emptySetData)
    return (readLines (lines contents) T emptySetData)

data LoadType = None | T | U
    deriving (Read, Show, Enum, Eq, Ord)

readLines :: [String] -> LoadType -> SetData -> SetData
readLines ("TRAITS":xs) _ setData = readLines xs T setData
readLines ("UNITS":xs) _ setData = readLines xs U setData
readLines (x:xs) T setData = readLines xs T (addTrait (newTrait x) setData)
readLines (x:xs) U setData = readLines xs U (addUnit (newUnit x) setData)
readLines _ _ setData = setData