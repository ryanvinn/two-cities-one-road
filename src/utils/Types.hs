module Utils.Types where

-- terreno do jogo
data Terrain = Plains | River | Mountain deriving (Eq, Show)

-- alias de tupla para cords para facilitar codigo
type Cords = (Int, Int)

-- tipo composto painel
data Tile = Tile
  { terrain :: Terrain, buildingCost :: Int, passingCost :: Int, location :: Cords, built :: Bool } deriving (Eq, Show)

-- alias de matriz para map para facilitar codigo
type Map = [[Tile]]
