module Graph where

-- importa coisas necessarias de types e game
import Types (Cords, Map, Tile(..))
import MapUtils

type Node = Cords
type Weight = Int
type Edge = (Node, Node, Weight)
type Graph = [Edge]
type Distance = [(Node, Int)]

-- cria um grafo que representa um mapa
buildGraph :: Map -> Graph
buildGraph mp = concatMap tileEdges allTiles
  where
    allTiles = concat mp
    tileEdges tile =
      let firstNode = location tile
          neighbors = findNeighbors firstNode mp
      in [(firstNode, secondNode, passingCost (getElement secondNode mp)) | secondNode <- neighbors, verifyCord secondNode mp]

listNodes :: Map -> [Node]
listNodes mp = map location (concat mp)

-- um "infinito" grande o suficiente
infinity :: Int
infinity = 1000000000

-- inicializa as distancias: 0 para "raiz", infinito para os demais
initialize :: [Node] -> Node -> Distance
initialize nodes src = [(n, if n == src then 0 else infinity) | n <- nodes]

-- retorna a distancia atual ate um vertice, infinito se nao for encontrado
getDist :: Node -> Distance -> Int
getDist _ [] = infinity
getDist v ((u,d):xs)
  | v == u = d
  | otherwise = getDist v xs

-- atualiza a distancia de v na lista (se for menor que a anterior)
updateDist :: Node -> Int -> Distance -> Distance
updateDist _ _ [] = []
updateDist v newVal ((u,d):xs)
  | v == v = (u, min d newVal) : xs
  | otherwise = (u, d) : updateDist v newVal xs

-- relaxa as arestas: atualiza a distancia entre vertices (crucial para o algoritmo)
relax :: Graph -> Distance -> Distance
relax [] dist = dist
relax ((u,v,w):es) dist =
  let du = getDist u dist
      dv = getDist v dist
  in if du + w < dv
        then relax es (updateDist v (du + w) dist)
        else relax es dist

-- repete o relaxamento n vezes. o algoritmo exige que o grafo seja relaxado |V| -1 vezes, |V| = numero de vertices
relaxNTimes :: Int -> Graph -> Distance -> Distance
relaxNTimes 0 _ dist = dist
relaxNTimes n graph dist = relaxNTimes (n-1) graph (relax graph dist)

-- verifica existencia de ciclos negativos
hasNegativeCycle :: Graph -> Distance -> Bool
hasNegativeCycle [] _ = False
hasNegativeCycle ((u,v,w):es) dist =
  let du = getDist u dist
      dv = getDist v dist
  in if du + w < dv
        then True
        else hasNegativeCycle es dist

-- algoritmo de bellman-ford. retorna distancia, se houver sucesso, ou uma mensagem de erro, se falhar.
-- Left e Right: vem do tipo Either, padrao de haskell, utilizado para capturar erros, possibilitando 2 tipos de retorno
bellmanFord :: [Node] -> Graph -> Node -> Either String Distance
bellmanFord nodes graph src =
  let dist0 = initialize nodes src
      distFinal = relaxNTimes (length nodes - 1) graph dist0
  in if hasNegativeCycle graph distFinal
        then Left "Ciclo negativo detectado!"
        else Right distFinal

