import World



-- Utility functions
getLevel :: Map -> Int
getLevel (Map mp) = ceiling $ (fromIntegral (width * height)) / 100
    where
        width  = sum $ map length mp
        height = length mp



calcScore :: World -> World -> Int
calcScore (World ((Map mp1), l1, g1, f1)) (World ((Map mp2), l2, g2, f2)) = multiplier * score
    where
        -- Are there any pills left?
        multiplier = if elem True $ map (\ts -> elem Pill ts) mp2
                     then 1
                     else (lives l1) + 1

        -- Calculate score for a single tile
        tilePoint :: Tile -> Tile -> Int
        tilePoint t1 t2 =
            case (t1, t2) of
                -- No change
                (Pill, Pill)   -> 0
                (Power, Power) -> 0
                (Fruit, Fruit) -> 0
                -- Eaten
                (Pill, _)      -> 10
                (Power, _)     -> 50
                (Fruit, _)     -> fruitPoints $ getLevel $ Map mp1
                -- Ignore
                _              -> 0

        tilePoints = sum $ zipWith (\x y -> sum $ zipWith tilePoint x y) mp1 mp2
        score = tilePoints -- + ghostPoints



fruitPoints :: Int -> Int
fruitPoints lvl =
    case lvl of
        1  -> 100
        2  -> 300
        3  -> 500
        4  -> 500
        5  -> 700
        6  -> 700
        7  -> 1000
        8  -> 1000
        9  -> 2000
        10 -> 2000
        11 -> 3000
        12 -> 3000
        _  -> 5000

