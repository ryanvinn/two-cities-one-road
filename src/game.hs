data Terrain = Plains | River | Mountain deriving (Eq, Show)
type Cords = (Int, Int)

data Tile = Tile { terrain :: Terrain, buildingCost :: Int, passingCost :: Int} deriving (Eq, Show)
-- funcoes seletoras: <atributo> <tipo>

type Map = [[Tile]]

createMap :: Cords -> Map
createMap (maxLat, maxLong) = [[tile lat long | lat <- [0..maxLat]] | long <- [0..maxLong]]
    where 
        tile lat long 
            | odd lat && even long = Tile Mountain 3 2
            | mod long 3 == 0 = Tile River 2 2
            | otherwise = Tile Plains 1 1

printTile :: Tile -> Char
printTile tile = case terrain tile of
  Plains   -> '.'
  River    -> '~'
  Mountain -> '^'

printMap :: Map -> IO ()
printMap = mapM_ (putStrLn . map printTile)

neighbors :: Tile -> [Tile, Tile, Tile, Tile]
neighbors curent =


main :: IO()
main = do
    let mapa = createMap (50, 5)
    printMap mapa
