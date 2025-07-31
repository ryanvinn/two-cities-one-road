-- definicao de tipos compostos e `alias` para facilitar legibilidade
data Terrain = Plains | River | Mountain deriving (Eq, Show)
type Cords = (Int, Int)
data Tile = Tile { terrain :: Terrain, buildingCost :: Int, passingCost :: Int, location :: Cords, built :: Bool} deriving (Eq, Show)
-- funcoes seletoras: <atributo> <tipo>
type Map = [[Tile]]

-- TODO: melhorar geracao de mapa (noise?)
createMap :: Cords -> Map
createMap (maxLat, maxLong) = [[tile lat long | lat <- [0..maxLat-1]] | long <- [0..maxLong-1]]
  where 
    tile lat long
      | odd lat && even long = Tile Mountain 3 2 (lat, long) False
      | mod long 3 == 0 = Tile River 2 2 (lat, long) False
      | otherwise = Tile Plains 1 1 (lat, long) False

-- TODO: escolher icones melhores (emojis?)
printTile :: Tile -> Char
printTile tile
  | built tile = 'O'
  | otherwise = case terrain tile of
    Plains -> '.'
    River -> '~'
    Mountain -> '^'

--printMap: mapeia a funcao printTile em todas as listas internas da matriz, e chama recursivamente para o restante
printMap :: Map -> IO ()
printMap [] = print "##########"
printMap (col: cols) = do
  putStrLn (map printTile col)
  printMap cols

getElement :: Cords -> Map -> Tile
getElement (lat, long) mp = mp !! lat !! long

findNeighbors :: Cords -> Map -> [Cords]
findNeighbors (lat, long) mp = [(lat -1, long), (lat +1, long), (lat, long -1), (lat, long +1)]

-- funcao build retorna o mesmo tile, so que com atributo de construido
buildTile :: Cords -> Map -> Tile
buildTile cords mp = newTile 
  where
    newTile = Tile (terrain current) (buildingCost current) (passingCost current) (location current) True 
      where current = getElement cords mp

main :: IO()
main = do
    let mapa = createMap (5, 5)
    print "##########"
    printMap mapa
