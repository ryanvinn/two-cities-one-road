module Map where

import System.Random (randomRIO)

import Persistence (writeString, readString, writeInt, readInt)

-- | Gera um novo mapa aleatório
generateRandomMap :: IO ()
generateRandomMap = do
  new_matrix <- getRandomMatrix
  new_header <- getRandomHeader

  writeString header_loc new_header
  writeString matrix_loc new_matrix

-- | Retorna informações de cabeçalho do mapa salvo
getGameHeader :: IO String
getGameHeader = readString header_loc

-- | Retorna informações de matriz do mapa salvo
getGameMatrix :: IO [String]
getGameMatrix = fmap sliceString (readString matrix_loc)

-- | Gera e retorna um mapa aleatório
getRandomMatrix :: IO String
getRandomMatrix = do
  middle <- sequence (replicate (lin*col - 2) (randomChar alphabet))
  return ("C" ++ middle ++ "C")
  where
    randomChar chars = do
      i <- randomRIO (0, length chars - 1)
      return (chars !! i)

-- | Gera e retorna um cabeçalho aleatório
getRandomHeader :: IO String
getRandomHeader = do
  randomNum <- randomRIO (cash_min, cash_max) :: IO Int
  return (show randomNum)

sliceString :: String -> [String]
sliceString s = [take 11 (drop (i * 11) s) | i <- [0..4]]

-- Alfabeto de porcentagens
alphabet :: String
alphabet = "pppPmMl"

-- | Localização do cabeçalho do mapa na memória
header_loc :: String
header_loc = "data/header.txt"

-- | Localização da matriz do mapa na memória
matrix_loc :: String
matrix_loc = "data/matrix.txt"

-- | Número mínimo de dinheiro para iniciar uma partida
cash_min :: Int
cash_min = 50

-- | Número máximo de dinheiro para iniciar uma partida
cash_max :: Int
cash_max = 150

-- Número de colunas
col :: Int
col = 5

-- Número de linhas
lin :: Int
lin = 11
