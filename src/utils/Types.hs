module Types where

--  Terreno
data Terrain = Plain | C_Plain | Lake | C_Lake | Mountain | C_Mountain | Forest | C_Forest | City | C_City deriving (Eq, Show)

-- Coordenada
type Cords = (Int, Int)

-- Painel (contendo terreno, custo de construção, custo de passagem, localização, estado de construção)
data Tile = Tile
  { terrain :: Terrain, buildingCost :: Int, passingCost :: Int, location :: Cords, built :: Bool } deriving (Eq, Show)

-- Matriz de painéis
type Map = [[Tile]]
