type Vertex = Int
type Weight = Int
type Edge = (Vertex, Vertex, Weight)
type Graph = [Edge]
type Distance = [(Vertex, Int)]

infinity :: Int
infinity = 1000000000  -- Um "infinito" grande o suficiente

-- Inicializa as distâncias: 0 para a fonte, infinito para os outros
initialize :: [Vertex] -> Vertex -> Distance
initialize vertices src = [ (v, if v == src then 0 else infinity) | v <- vertices ]

-- Atualiza a distância de um vértice na lista
updateDist :: Vertex -> Int -> Distance -> Distance
updateDist v newVal [] = []
updateDist v newVal ((u,d):xs)
  | u == v    = (u, min d newVal) : xs
  | otherwise = (u, d) : updateDist v newVal xs

-- Procura a distância atual de um vértice
getDist :: Vertex -> Distance -> Int
getDist _ [] = infinity
getDist v ((u,d):xs)
  | v == u    = d
  | otherwise = getDist v xs

-- Relaxa todas as arestas
relax :: Graph -> Distance -> Distance
relax [] dist = dist
relax ((u,v,w):es) dist =
  let du = getDist u dist
      dv = getDist v dist
  in if du + w < dv
        then relax es (updateDist v (du + w) dist)
        else relax es dist

-- Repete relaxamento n vezes
relaxNTimes :: Int -> Graph -> Distance -> Distance
relaxNTimes 0 _ dist = dist
relaxNTimes n graph dist = relaxNTimes (n-1) graph (relax graph dist)

-- Verifica se há ciclo negativo
hasNegativeCycle :: Graph -> Distance -> Bool
hasNegativeCycle [] _ = False
hasNegativeCycle ((u,v,w):es) dist =
  let du = getDist u dist
      dv = getDist v dist
  in if du + w < dv
        then True
        else hasNegativeCycle es dist

-- Função principal
bellmanFord :: [Vertex] -> Graph -> Vertex -> Either String Distance
bellmanFord vertices graph src =
  let dist0 = initialize vertices src
      distFinal = relaxNTimes (length vertices - 1) graph dist0
  in if hasNegativeCycle graph distFinal
        then Left "Ciclo negativo detectado!"
        else Right distFinal

main :: IO ()
main = do
  let vertices = [0,1,2,3]
      graph = [ (0,1,1)
              , (1,2,3)
              , (0,2,10)
              , (2,3,8)
              ]
      result = bellmanFord vertices graph 0
  case result of
    Left err -> putStrLn err
    Right dist -> mapM_ print dist
