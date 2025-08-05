module Main where

import Data.List (intercalate) -- Para intercalar dados na formatação
import System.Console.Haskeline -- Para capturar comandos de teclado

-- | Função principal do programa
main :: IO()
main = runHomeScreen

-- Executa a tela inicial
runHomeScreen :: IO()
runHomeScreen = do
  clearScreen
  putStr homeScreen
  key <- getKey
  case key of
    'a' -> runHomeScreen -- TODO
    's' -> runMapSelectionScreen
    'g' -> runHomeScreen -- TODO
    'q' -> return ()
    _ -> runHomeScreen

-- Executa a tela de seleção de mapas
runMapSelectionScreen :: IO()
runMapSelectionScreen = do
  clearScreen
  putStr mapSelectionScreen
  key <- getKey
  case key of
    '0' -> runMapVisualizationScreen 0 -- TODO
    -- TODO
    'q' -> runHomeScreen
    _ -> runMapSelectionScreen

-- | Executa tela de visualização de mapa
runMapVisualizationScreen :: Int -> IO ()
runMapVisualizationScreen mapNumber = do
  clearScreen
  putStr (mapVisualizationScreen mapNumber)
  key <- getKey
  case key of
    'j' -> runGameScreen mapNumber
    'q' -> runMapSelectionScreen
    _ -> runMapVisualizationScreen mapNumber

-- | Executa tela de jogo
runGameScreen :: Int -> IO ()
runGameScreen mapNumber = do
  clearScreen
  putStr (gameScreen mapNumber)
  key <- getKey
  case key of
    -- TODO
    'q' -> runHomeScreen
    _ -> runGameScreen mapNumber

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
    "(A) Acessar o último mapa jogado",
    "(S) Selecionar mapas",
    "(G) Gerar e acessar um novo mapa",
    "(Q) Encerrar jogo"
  ])
  (middleJustifyLine "<Aperte a tecla correspondente à uma das opções>")

-- | Retorna uma String representativa da tela de seleção de mapas
-- Para tanto, busca na memória os 10 últimos mapas com jogos não finalizados
mapSelectionScreen :: String
mapSelectionScreen = baseScreen
  (middleJustifyLine "Seleção de mapas")
  (emptyMiddle) -- TODO exibir 10 mapas (duas linhas por mapa)
  (middleJustifyLine "<0-9> para abrir mapa <Q> para voltar à tela anterior")

-- |  Retorna uma String representativa da tela de visualização de mapa
-- Para tanto, a partir do número do mapa, busca o arquivo recpectivo na memória
-- (com os valores da matriz 5x11 e os valores dos recursos)
mapVisualizationScreen :: Int -> String
mapVisualizationScreen map_number = baseScreen
  (emptyLine) -- TODO exibir recursos, tal qual na tela de jogo
  (emptyMiddle) -- TODO exibir estado do mapa
  (middleJustifyLine "<J> para jogar mapa <Q> para voltar à tela anterior")

-- | Retorna uma String representativa da tela de jogo
-- Para tanto, a partir do número do mapa, busca o arquivo recpectivo na memória
-- (com os valores da matriz 5x11 e os valores dos recursos)
gameScreen :: Int -> String
gameScreen map_number = baseScreen
  (emptyLine) -- TODO
  (emptyMiddle) -- TODO
  (emptyLine) -- TODO

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
