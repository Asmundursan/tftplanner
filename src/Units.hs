module Units where

data Unit = Unit {
    name :: UnitName
    , cost :: Int
    , traits :: TraitList
    }
    deriving(Read, Eq)

instance Show Unit where
    show unit = "\n" ++ Units.name unit ++ " " ++ show (cost unit) ++ " " ++ showTraitList (traits unit)
        where showTraitList (t:traits)
                | null traits = t
                | otherwise = t ++ " " ++ showTraitList traits

type UnitName = String
type TraitList = [String]

newUnit :: String -> Unit
newUnit text = Unit{Units.name = getName text, cost = getCost text, traits = getTraits text}
    where
         getName = head . words
         getCost = read . head . tail . words
         getTraits = tail . tail . words