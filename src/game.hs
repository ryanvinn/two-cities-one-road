-- definicao de tipos compostos e `alias` para facilitar legibilidade
data Terrain = Plains | River | Mountain deriving (Eq, Show)
type Cords = (Int, Int)
data Tile = Tile { terrain :: Terrain, buildingCost :: Int, passingCost :: Int, location :: Cords, built :: Bool} deriving (Eq, Show)
-- funcoes seletoras: <atributo> <tipo>
type Map = [[Tile]]

-- TODO: melhorar geracao de mapa (noise?)
createMap :: Cords -> Map
createMap (maxLat, maxLong) = [[tile lat long | long <- [0..maxLong-1]] | lat <- [0..maxLat-1]]
  where 
    tile lat long
      | odd lat && even long = Tile Mountain 3 2 (lat, long) False
      | mod long 3 == 0 = Tile River 2 2 (lat, long) False
      | otherwise = Tile Plains 1 1 (lat, long) False

verifyCord :: Cords -> Map -> Bool
verifyCord (lat, long) mp =
  lat >= 0 && lat < length mp &&
  long >= 0 && long < length (head mp)

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

updateGrid :: Cords -> Map -> Map
updateGrid (lat, long) mp =
  let row = mp !! lat
      newRow = take long row ++ [buildTile (lat, long) mp] ++ drop (long + 1) row
  in take lat mp ++ [newRow] ++ drop (lat + 1) mp

readPlayer :: String -> Maybe Cords
readPlayer input = 
  case words input of [a, b] -> 
    case (reads a, reads b) of ((lat, ""):_, (long, ""):_) -> 
    Just (lat, long)
    _ -> Nothing
  _ -> Nothing

main :: IO()
main = do
    let mapa = createMap (5, 5)
    print "##########"
    printMap mapa

    let mapa2 = updateGrid (2, 3) mapa

    printMap mapa2

    --print (getElement (2, 3) mapa)
    --print (getElement (3, 2) mapa)
