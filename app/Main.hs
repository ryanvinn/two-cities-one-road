module Main where

import System.Console.Haskeline -- Para capturar comandos de teclado

import Persistence (verifyAndCreateFiles)
import Interfaces (homeScreen, gameScreen)
import Map (generateRandomMap)

-- | Função principal do programa
main :: IO()
main = do
  verifyAndCreateFiles
  runHomeScreen

-- Executa a tela inicial
runHomeScreen :: IO()
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
  -- TODO interface com lógica do jogo
  putStr gameScreen
  key <- getKey
  case key of
    'q' -> runHomeScreen
    _ -> runGameScreen

-- | Limpa a tela
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- Retorna em Char a tecla pressionada
getKey :: IO Char
getKey = do
  result <- runInputT defaultSettings (getInputChar "")
  case result of
    Just c -> return c
    Nothing -> return '\0'
