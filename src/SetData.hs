module SetData where
import Traits
import Units
import Data.Char (toLower)

data SetData = SetData {
    traits :: [Trait]
    , units :: [Unit]
    }
    deriving(Read)
instance Show SetData where
    show setData = "TRAITS" ++ show (SetData.traits setData) ++ "\nUNITS" ++ show (units setData) ++ "\n"


emptySetData :: SetData
emptySetData = SetData {SetData.traits = [], SetData.units = []}

addTrait :: Trait -> SetData -> SetData
addTrait trait std = std{SetData.traits = trait : SetData.traits std }

addUnit :: Unit -> SetData -> SetData
addUnit unit std = std{SetData.units = unit : SetData.units std }

--toLower :: TraitName -> TraitName
--toLower (TraitName x) = toLower x

getTrait :: TraitName -> SetData -> Maybe Trait
getTrait name std = gTrait name (SetData.traits std) 
    where gTrait :: TraitName -> [Trait] -> Maybe Trait
          gTrait name [] = Nothing
          gTrait name (x:xs) 
            | map toLower name == map toLower (Traits.name x) = Just x
            | map toLower name /= map toLower (Traits.name x) = gTrait name xs
            | otherwise = Nothing

getUnit :: UnitName -> SetData -> Maybe Unit
getUnit name std = gUnit name (SetData.units std)
    where gUnit :: UnitName -> [Unit] -> Maybe Unit
          gUnit name [] = Nothing
          gUnit name (x:xs) 
            | map toLower name == map toLower (Units.name x) = Just x
            | map toLower name /= map toLower (Units.name x) = gUnit name xs
            | otherwise = Nothing