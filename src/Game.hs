module Game where

import Types
import Graph
import MapUtils
import Map (getPlayerCash, getPlayerCoord, getMapMatrix)
import Map (setPlayerCash, setPlayerCoord, setMapMatrix)

-- Loop de execução do jogo
gameLoop :: Char -> IO Int
gameLoop key = do
  coordStr <- getPlayerCoord
  let player_coord = StringToCoord coordStr
  let next_coord = charToNextCoord key player_coord
  let map_matrix = stringToMap getMapMatrix

  let available = getAvailableBuilds player_coord map_matrix
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

-- processa informacoes do painel para exibir na tela
printTile :: Tile -> [Coord] -> Char
printTile tile available
  | built tile = 'O'
  | location tile `elem` available = '*'
  | otherwise = case terrain tile of
      Plains -> '.'
      River -> '~'
      Mountain -> '^'

-- printMap: mapeia a funcao printTile em todas as listas internas da matriz, e chama recursivamente para o restante
printMap :: Map -> [Coord] -> IO ()
printMap [] _ = print "##########"
printMap (col:cols) available = do
  putStrLn (map (\tile -> printTile tile available) col)
  printMap cols available

-- funcao build retorna o mesmo tile, so que com atributo de construido
buildTile :: Coord -> Map -> Tile
buildTile cords mp = newTile 
  where
    newTile = Tile (terrain current) (buildingCost current) (passingCost current) (location current) True 
      where current = getElement cords mp

-- atualiza o mapa quando algum tile for construido
buildOnMap :: Coord -> Map -> Map
buildOnMap (lat, long) mp =
  let row = mp !! lat
      newRow = take long row ++ [buildTile (lat, long) mp] ++ drop (long + 1) row
  in take lat mp ++ [newRow] ++ drop (lat + 1) mp

-- verifica se um painel esta disponivel, ou ja foi construido
canBuild :: Coord -> Map -> Bool
canBuild cords mp = 
  verifyCord cords mp && not (built (getElement cords mp))

-- aplica o filtro de canBuild na lista de vizinhos de um painel
getAvailableBuilds :: Coord -> Map -> [Coord]
getAvailableBuilds cords mp = 
  filter (\c -> canBuild c mp) (findNeighbors cords mp)

-- verifica se chegou ao painel destino
arrivedAt :: Coord -> Coord -> Map -> Bool
arrivedAt current endCity mp = endCity `elem` (findNeighbors current mp)

-- funcao auxiliar para evitar erros ao ler inteiros da do jogador
readInputInt :: String -> Maybe Int
readInputInt s = case reads s of
  [(n, "")] -> Just n
  _-> Nothing

-- le e interpreta a entrada do usuario em coordenadas
readPlayer :: String -> Maybe Coord
readPlayer input =
  case words input of
    [a, b] -> case (readInputInt a, readInputInt b) of
      (Just lat, Just long) -> Just (lat, long)
      _-> Nothing
    _-> Nothing

getNodes :: Map -> [Coord]
getNodes mp = map location (concat mp)

-- funcoes de IO().
-- funcao que avalia o custo do trajeto do jogador
evaluatePath :: [Coord] -> Map -> Coord -> Coord -> IO()
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
