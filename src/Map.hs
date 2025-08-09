module Map where

import System.Random (randomRIO)

import Persistence (writeString, readString, writeInt, readInt)

-- | Gera um novo mapa aleatório
generateRandomMap :: IO ()
generateRandomMap = do
  new_matrix <- getRandomMatrix
  new_header <- getRandomHeader

  writeString "data/matrix.txt" new_matrix
  writeString "data/header.txt" new_header

-- | Retorna informações dos mapas salvos em memória
getSavedMapsInfos :: [String]
getSavedMapsInfos = [""] -- TODO

-- | Retorna informações de cabeçalho do mapa em questão
getGameHeader :: String
getGameHeader = "" -- TODO

-- | Retorna informações de matriz do mapa em questão
getGameMatrix :: [String]
getGameMatrix = [""] -- TODO

-- | Gera e retorna um mapa aleatório
getRandomMatrix :: IO String
getRandomMatrix = do
  middle <- sequence (replicate (11*5 - 2) (randomChar alphabet))
  return ("C" ++ middle ++ "C")
  where
    randomChar chars = do
      i <- randomRIO (0, length chars - 1)
      return (chars !! i)
    alphabet = "PPPPMML"

-- | Gera e retorna um cabeçalho aleatório
getRandomHeader :: IO String
getRandomHeader = do
  randomNum <- randomRIO (50, 150) :: IO Int
  return (show randomNum)
