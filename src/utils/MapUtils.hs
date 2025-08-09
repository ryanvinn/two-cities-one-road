module Utils.MapUtils where

import Utils.Types

-- pega um tile a partir das coordenadas
getElement :: Cords -> Map -> Tile
getElement (lat, long) mp = mp !! lat !! long

verifyCord :: Cords -> Map -> Bool
verifyCord (lat, long) mp =
  lat >= 0 && lat < length mp &&
  long >= 0 && long < length (head mp)

-- retorna os 4 vizinhos (norte sul leste oeste), independente de existirem
findNeighbors :: Cords -> Map -> [Cords]
findNeighbors (lat, long) _ = 
  [(lat - 1, long), (lat + 1, long), (lat, long - 1), (lat, long + 1)]
