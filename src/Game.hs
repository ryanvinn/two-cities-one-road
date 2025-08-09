module Src.Game where

import Utils.Types
import Utils.Graph
import Utils.MapUtils

-- gera o mapa do jogo
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

getNodes :: Map -> [Cords]
getNodes mp = map location (concat mp)

-- funcoes de IO().
-- funcao que avalia o custo do trajeto do jogador
evaluatePath :: [Cords] -> Map -> Cords -> Cords -> IO()
evaluatePath playerPath mp start end = do
  let startCost = passingCost (getElement start mp)
  let endCost = passingCost (getElement end mp)
  let playerCost = sum (map (\cords -> passingCost (getElement cords mp)) playerPath) - startCost

  let nodes = getNodes mp
  let graph = buildGraph mp

  case bellmanFord nodes graph start of
    Left err -> putStrLn ("Erro ao computar caminho: " ++ err)
    Right (distances, preds) -> case lookup end distances of

      Nothing -> putStrLn "Erro: Destino inalcançável"
      
      Just optimalCost -> do
        let bestPath = buildPath start end preds
        putStrLn ("Sua ferrovia passa por: " ++ show playerPath)
        putStrLn ("Seus custos: " ++ show playerCost)
        putStrLn ("Ferrovia ideal (Bellman-Ford): " ++ show bestPath)
        putStrLn ("Custo ideal (Bellman-Ford): " ++ show optimalCost)
        
        let diff = playerCost - optimalCost
        if diff == 0
          then putStrLn "Trabalho perfeito! Você fez a ferrovia mais rápida possível!"
        
        else putStrLn ("Viagens custam " ++ show diff ++ " a mais que o melhor trajeto possível.")

-- loop que executa o jogo interativamente com o jogador
gameLoop :: Map -> Cords -> Cords -> Int -> [Cords] -> Cords -> IO ()
gameLoop mp currentTile secondCity budget path firstCity = do
  let available = getAvailableBuilds currentTile mp
  let affordable = filter (\c -> buildingCost (getElement c mp) <= budget) available
  
  -- verifica casos de derrota
  if null available then do
    printMap mp []
    putStrLn "Você perdeu! Não existem painéis livres."

  else if null affordable then do
    printMap mp []
    putStrLn "Você perdeu! Orçamento insuficiente."

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
                putStrLn ("Não é possível construir aqui. Você tem: $" ++ show budget ++ ". Necessário: $" ++ show cost)
                gameLoop mp currentTile secondCity budget path firstCity

              -- construir e verificar se chegou ao fim
              else do
                let newMap = buildOnMap cords mp
                let newPath = path ++ [cords]
                putStrLn ("Construído em: " ++ show cords ++ "! (Orçamento restante: $" ++ show currentBudget ++ " )")

                if (arrivedAt cords secondCity mp) then do
                  printMap newMap []
                  putStrLn "Cidades conectadas!"
                  let finalPath = newPath ++ [secondCity]
                  evaluatePath finalPath mp firstCity secondCity
                else 
                    gameLoop newMap cords secondCity currentBudget newPath firstCity

          -- processa entradas invalidas      
          else do
            putStrLn "Escolha um painel vizinho ao último construído!"
            gameLoop mp currentTile secondCity budget path firstCity
          
          Nothing -> do
            putStrLn "Comando inválido! Tente novamente."
            gameLoop mp currentTile secondCity budget path firstCity

-- main
main :: IO ()
main = do
  let mapSize = (5, 11)
  let startLat = 2
  let endLat = 4
  let initialMap = createMap mapSize (startLat, endLat)
  let firstCity = (startLat, 0)
  let secondCity = (endLat, 10)
  let initialBudget = 20
  gameLoop initialMap firstCity secondCity initialBudget [firstCity] firstCity
