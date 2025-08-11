module MapUtils where

import Types
import Data.Char (toUpper)
import Map (getPlayerCoord)

-- pega um tile a partir das coordenadas
getElement :: Coord -> Map -> Tile
getElement (lat, long) mp = mp !! lat !! long

verifyCoord :: Coord -> Map -> Bool
verifyCoord (lat, long) mp =
  lat >= 0 && lat < length mp &&
  long >= 0 && long < length (head mp)

-- retorna os 4 vizinhos (norte sul leste oeste), independente de existirem
findNeighbors :: Coord -> Map -> [Coord]
findNeighbors (lat, long) _ = 
  [(lat - 1, long), (lat + 1, long), (lat, long - 1), (lat, long + 1)]

-- | Converte String em Coord
stringToCoord :: String -> Coord
stringToCoord [x, y] = (hexCharToInt x, hexCharToInt y)

-- | Recebe key (Char) e Coord antiga e converte em próxima Coord
charToNextCoord :: Char -> Coord -> Coord
charToNextCoord c (x, y) = case c of
  'W' -> (x + 1, y + 1)
  'A' -> (x - 1, y + 1)
  'S' -> (x - 1, y - 1)
  'D' -> (x + 1, y - 1)
  _ -> (x, y)

-- | Converte [String] em Map
stringsToMap :: [String] -> Map
stringsToMap strs =  map (map charToTile) (zipWith (\i -> zipWith (\j c -> (i,j,c)) [0..]) [0..] strs)
  where
    charToTile (i,j,c) = case of
      'p' -> Tile Plains 10 1 (i,j) False
      'P' -> Tile C_Plain 10 1 (i,j) True
      -- TODO
      _ -> Tile Plains 10 1 (i,j) False

-- Função para converter um caractere hexadecimal em um inteiro
hexCharToInt :: Char -> Int
hexCharToInt c
    | c >= '0' && c <= '9' = ord (toUpper c) - ord '0'
    | c >= 'A' && c <= 'F' = ord (toUpper c) - ord 'A' + 10
    | otherwise = error " "
