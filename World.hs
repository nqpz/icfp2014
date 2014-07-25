module World where

import Data.List (intercalate)

-- Encodings:
-- Health status:
-- 0: standard
-- n: n ticks left of power pill
type Health = Int
type Lives = Int
data Direction = Up | Right | Down | Left
type Score = Int
type Location = (Int, Int)
data Tile = Wall | Empty | Pill | Power | Fruit | Lambda | Ghost
    deriving (Eq, Show, Ord)

-- Encoded map
newtype Map = Map [[Tile]]

instance Show Map where
    show (Map mp) = show $ intercalate "\n" (map stringify mp)
        where
            stringify :: [Tile] -> String
            stringify l = concat $ map cnv l
            cnv n = case n of
                        Wall    -> "#"
                        Empty   -> " "
                        Pill    -> "."
                        Power   -> "o"
                        Fruit   -> "%"
                        Lambda  -> "\\"
                        Ghost   -> "="

data LambdaManStatus = Man { health    :: Health
                       , location  :: Location
                       , direction :: Direction
                       , lives     :: Lives
                       , score     :: Score
                       }

-- List of all ghosts
type GhostStatus = [(Health, Location, Direction)]

-- Fruit status:
-- 0: no fruit present
-- n: n ticks left of fruit being present
type FruitLocationStatus = Int


-- Combined world encoding
newtype World = World (Map, LambdaManStatus, GhostStatus, FruitLocationStatus)
