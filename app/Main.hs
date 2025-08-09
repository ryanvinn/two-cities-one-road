module Main where

import Data.List (intercalate) -- Para intercalar dados na formatação
import System.Console.Haskeline -- Para capturar comandos de teclado

import Map
import Persistence

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
  putStr gameScreen
  key <- getKey
  case key of
    'q' -> runHomeScreen
    _ -> runGameScreen

-- | Retorna uma String representativa da tela inicial
homeScreen :: String
homeScreen = baseScreen
  (middleJustifyLine "Two Cities One Road")
  (topJustifyColumn [
    "Bem vindo ao jogo!",
    "",
    "Seu objetivo é conectar duas cidades com recursos",
    "limitados e diversos obstáculos e materiais.",
    "",
    "(A) Acessar último mapa",
    "(G) Gerar novo mapa",
    "(Q) Encerrar programa"
  ])
  (middleJustifyLine "<Aperte a tecla correspondente à uma das opções>")

-- | Retorna uma String representativa da tela de jogo
-- Para tanto, a partir do número do mapa, busca o arquivo recpectivo na memória
-- (com os valores da matriz 5x11 e os valores dos recursos)
gameScreen :: String
gameScreen = baseScreen
  (leftJustifyLine getGameHeader)
  (middleJustifyColumn getGameMatrix)
  (emptyLine) -- TODO exibir comandos possíveis

-- | Recebe os valores de cima, baixo e centro da tela e retorna a tela
-- formatada
baseScreen :: String -> [String] -> String -> String
baseScreen top middle down = unlines [
    ("╭" ++ simpleLine ++ "╮"),
    ("│" ++ top ++ "│"),
    ("├" ++ simpleLine ++ "┤"),
    (middleWithEdges middle),
    ("├" ++ simpleLine ++ "┤"),
    ("│" ++ down ++ "│"),
    ("╰" ++ simpleLine ++ "╯")
  ]

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

-- | Retorna um linha de 55 caracteres '─'
simpleLine :: String
simpleLine = replicate line_length '─'

-- | Retorna uma linha de 55 caracteres ' '
emptyLine :: String
emptyLine = replicate line_length ' '

-- | Retorna uma série de 15 linhas em branco
emptyMiddle :: [String]
emptyMiddle = replicate column_length emptyLine 

-- | Retorna uma seção com o conteúdo passado e uma borda
middleWithEdges :: [String] -> String
middleWithEdges middle = intercalate "\n" (map (\x -> "│" ++ x ++ "│") middle)

-- | Justifica o conteúdo de uma linha no meio
middleJustifyLine :: String -> String
middleJustifyLine str = replicate left ' ' ++ str ++ replicate right ' '
  where
    len = length str
    remain = line_length - len
    left = div remain 2
    right = remain - left

-- | Justifica o conteúdo de uma coluna no meio
middleJustifyColumn :: [String] -> [String]
middleJustifyColumn strs = before ++ left_justified_lines ++ after
  where
    lines_number = length strs
    lines_number_before = div (column_length - lines_number) 2
    lines_number_after = column_length - lines_number - lines_number_before
    before = replicate lines_number_before emptyLine
    after = replicate lines_number_after emptyLine

    left_justified_lines = map leftJustifyLine strs

-- | Justifica o conteúdo de uma coluna em cima
topJustifyColumn :: [String] -> [String]
topJustifyColumn strs = left_justified_lines ++ after
  where
    lines_number = length strs
    lines_number_after = column_length - lines_number
    after = replicate lines_number_after emptyLine

    left_justified_lines = map leftJustifyLine strs

-- | Justifica o conteúdo de uma linha à esquerda
leftJustifyLine :: String -> String
leftJustifyLine str = str ++ replicate (line_length - length str) ' '

-- | Retorna o tamanho de uma coluna
column_length :: Int
column_length = 15

-- | Retorna o tamanho de uma linha
line_length :: Int
line_length = 55
