module Units where
import Traits
import Text.Read

data Unit = Unit {
    name :: UnitName
    , cost :: Int
    , traits :: TraitList
    }
    deriving(Read, Eq)

instance Show Unit where
    show unit = "\n" ++ Units.name unit ++ " " ++ show (cost unit) ++ " " ++ showTraitList (traits unit)
        where showTraitList (t:traits) 
                | traits == [] = t
                | otherwise = t ++ " " ++ showTraitList traits
    
type UnitName = String
type TraitList = [String]

newUnit :: String -> Unit
newUnit text = Unit{Units.name = getName text, cost = getCost text, traits = getTraits text}

getCost :: String -> Int
getCost [] =  0
getCost (x:xs) = case readMaybe [x] of
    Nothing -> getCost xs 
    Just i -> i


getTraits :: String -> TraitList
getTraits text = tail(tail(words text))