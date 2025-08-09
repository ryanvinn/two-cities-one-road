module Map where

import System.Random (randomRIO)

import Persistence

-- | Gera e retorna numeração de um novo mapa
generateRandomMap :: Int
generateRandomMap = 0 -- TODO fila circular

-- | Retorna informações dos mapas salvos em memória
getSavedMapsInfos :: [String]
getSavedMapsInfos = [""] -- TODO

-- | Retorna informações de cabeçalho do mapa em questão
getGameHeader :: Int -> String
getGameHeader map_number = "" -- TODO

-- | Retorna informações de matriz do mapa em questão
getGameMatrix :: Int -> [String]
getGameMatrix map_number = [""] -- TODO

-- | Gera e retorna um mapa aleatório
getRandomMap :: IO String
getRandomMap = do
  middle <- sequence (replicate (11*5 - 2) (randomChar alphabet))
  return ("C" ++ middle ++ "C")
  where
    randomChar chars = do
      i <- randomRIO (0, length chars - 1)
      return (chars !! i)
    alphabet = "PPPPMML"
