module Game where

-- definicao de tipos compostos e `alias` para facilitar legibilidade
data Terrain = Plains | River | Mountain deriving (Eq, Show)
type Cords = (Int, Int)
data Tile = Tile { terrain :: Terrain, buildingCost :: Int, passingCost :: Int, location :: Cords, built :: Bool} deriving (Eq, Show)
-- funcoes seletoras: <atributo> <tipo>
type Map = [[Tile]]

-- TODO: melhorar geracao de mapa (noise?)
createMap :: Cords -> (Int, Int) -> Map
createMap (maxLat, maxLong) (startLat, endLat) =
  let baseMap = [[tile lat long | long <- [0..maxLong-1]] | lat <- [0..maxLat-1]]
      firstCity = buildOnMap (startLat, 0) baseMap
      secondCity = buildOnMap (endLat, maxLong - 1) firstCity
  in secondCity
  where 
    tile lat long
      | odd lat && even long = Tile Mountain 3 2 (lat, long) False
      | mod long 3 == 0 = Tile River 2 2 (lat, long) False
      | otherwise = Tile Plains 1 1 (lat, long) False

verifyCord :: Cords -> Map -> Bool
verifyCord (lat, long) mp =
  lat >= 0 && lat < length mp &&
  long >= 0 && long < length (head mp)

-- processa informacoes do painel para exibir na tela
printTile :: Tile -> [Cords] -> Char
printTile tile available
  | built tile = 'O'
  | location tile `elem` available = '*'
  | otherwise = case terrain tile of
      Plains -> '.'
      River -> '~'
      Mountain -> '^'

-- printMap: mapeia a funcao printTile em todas as listas internas da matriz, e chama recursivamente para o restante
printMap :: Map -> [Cords] -> IO ()
printMap [] _ = print "##########"
printMap (col:cols) available = do
  putStrLn (map (\tile -> printTile tile available) col)
  printMap cols available

getElement :: Cords -> Map -> Tile
getElement (lat, long) mp = mp !! lat !! long

-- retorna uma lista com as coordenadas dos 4 vizinhos de um tile 
findNeighbors :: Cords -> Map -> [Cords]
findNeighbors (lat, long) mp = [(lat -1, long), (lat +1, long), (lat, long -1), (lat, long +1)]

-- funcao build retorna o mesmo tile, so que com atributo de construido
buildTile :: Cords -> Map -> Tile
buildTile cords mp = newTile 
  where
    newTile = Tile (terrain current) (buildingCost current) (passingCost current) (location current) True 
      where current = getElement cords mp

-- atualiza o mapa quando algum tile for construido
buildOnMap :: Cords -> Map -> Map
buildOnMap (lat, long) mp =
  let row = mp !! lat
      newRow = take long row ++ [buildTile (lat, long) mp] ++ drop (long + 1) row
  in take lat mp ++ [newRow] ++ drop (lat + 1) mp

-- verifica se um painel esta disponivel, ou ja foi construido
canBuild :: Cords -> Map -> Bool
canBuild cords mp = 
  verifyCord cords mp && not (built (getElement cords mp))

-- aplica o filtro de canBuild na lista de vizinhos de um painel
getAvailableBuilds :: Cords -> Map -> [Cords]
getAvailableBuilds cords mp = 
  filter (\c -> canBuild c mp) (findNeighbors cords mp)

-- verifica se chegou ao painel destino
arrivedAt :: Cords -> Cords -> Map -> Bool
arrivedAt current endCity mp = endCity `elem` (findNeighbors current mp)

-- funcao auxiliar para evitar erros ao ler inteiros da do jogador
readInputInt :: String -> Maybe Int
readInputInt s = case reads s of
  [(n, "")] -> Just n
  _-> Nothing

-- le e interpreta a entrada do usuario em coordenadas
readPlayer :: String -> Maybe Cords
readPlayer input =
  case words input of
    [a, b] -> case (readInputInt a, readInputInt b) of
      (Just lat, Just long) -> Just (lat, long)
      _-> Nothing
    _-> Nothing

-- loop que executa o jogo interativamente com o jogador
gameLoop :: Map -> Cords -> Cords -> Int -> IO ()
gameLoop mp currentTile secondCity budget = do
  let available = getAvailableBuilds currentTile mp
  
  -- verifica casos de derrota
  if null available then do
    printMap mp []
    putStrLn "Você perdeu! Não existem painéis livres."

  else if budget <= 0 then do
    printMap mp []
    putStrLn "Você perdeu! Seu orçamento acabou."

    else do
      printMap mp available
      putStrLn "Escolha LINHA e COLUNA do painel possível que você quer construir (ex: 1 1) ou digite 'q' para sair."
      input <- getLine

      -- processamento da entrada
      case input of
        "q" -> putStrLn "Jogo interrompido."

        _ -> case readPlayer input of
          Just cords ->
            if cords `elem` available then do
              let tile = (getElement cords mp) 
              let cost = buildingCost tile
              let currentBudget = budget - cost

              -- analisa orçamento 
              if currentBudget < 0 then do 
                putStrLn $ "Não é possível construir aqui. Você tem: $" ++ show budget ++ ". Necessário: $" ++ show cost
                gameLoop mp currentTile secondCity budget

              -- construir e verificar se chegou ao fim
              else do
                let newMap = buildOnMap cords mp
                putStrLn $ "Construído em: " ++ show cords ++ "! (Orçamento restante: $" ++ show currentBudget ++ " )"

                if (arrivedAt cords secondCity mp) then do
                  printMap newMap []
                  putStrLn "Cidades conectadas!"
                else 
                    gameLoop newMap cords secondCity currentBudget

          -- processa entradas invalidas      
          else do
            putStrLn "Escolha um painel vizinho ao último construído!"
            gameLoop mp currentTile secondCity budget
          
          Nothing -> do
            putStrLn "Comando inválido! Tente novamente."
            gameLoop mp currentTile secondCity budget

-- main
main :: IO ()
main = do
  let mapSize = (5, 11)
  let startLat = 2
  let endLat = 4
  let initialMap = createMap mapSize (startLat, endLat)
  let firstCity = (startLat, 0)
  let secondCity = (endLat, 10)
  let initialBudget = 15
  gameLoop initialMap firstCity secondCity initialBudget
