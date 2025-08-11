module Main where

import Persistence (verifyAndCreateFiles)
import Interfaces (homeScreen, gameScreen)
import Map (generateRandomMap)
import Game (gameLoop)
import IOUtils (getKey, clearScreen)

-- | Função principal do programa
main :: IO ()
main = do
  verifyAndCreateFiles
  runHomeScreen

-- Executa a tela inicial
runHomeScreen :: IO ()
runHomeScreen = do
  clearScreen
  putStr homeScreen
  key <- getKey
  case key of
    'a' -> runGameScreen
    'g' -> do
      generateRandomMap
      runGameScreen
    'q' -> return ()
    _ -> runHomeScreen

-- | Executa tela de jogo
runGameScreen :: IO ()
runGameScreen = do
  clearScreen
  screen <- gameScreen
  putStr screen
  key <- getKey
  case key of
    'q' -> runHomeScreen
    'w' -> handleMovement 'w'
    'a' -> handleMovement 'a'
    's' -> handleMovement 's'
    'd' -> handleMovement 'd'
    _ -> runGameScreen

handleMovement :: Char -> IO ()
handleMovement direction = do
  out <- gameLoop direction
  if out == 0
    then runHomeScreen
    else runGameScreen
