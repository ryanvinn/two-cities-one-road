module Main where

import Data.List (intercalate) -- Para intercalar dados na formatação
import System.Console.Haskeline -- Para capturar comandos de teclado

column_length :: Int
column_length = 15

line_length :: Int
line_length = 55

-- | Função principal do programa
main :: IO()
main = putStr ""

-- Retorna em Char a tecla pressionada
getKey :: IO Char
getKey = runInputT defaultSettings getInputChar

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
    "(A)cessar o último mapa jogado",
    "(S)elecionar mapas",
    "(G)erar e acessar um novo mapa",
    "(E)ncerrar jogo"
  ])
  (middleJustifyLine "<Aperte a tecla correspondente à uma das opções>")

-- | Retorna uma String representativa da tela de seleção de mapas
mapSelectionScreen :: String
mapSelectionScreen = baseScreen
  (middleJustifyLine "Seleção de mapas")
  (emptyMiddle) -- TODO exibir 10 mapas (duas linhas por mapa)
  (middleJustifyLine "<0-9> para abrir mapa <Q> para voltar à tela anterior")

-- |  Retorna uma String representativa da tela de visualização de mapa
mapVisualizationScreen :: String
mapVisualizationScreen = baseScreen
  (emptyLine) -- TODO exibir recursos, tal qual na tela de jogo
  (emptyMiddle) -- TODO exibir estado do mapa
  (middleJustifyLine "<J> para jogar mapa <Q> para voltar à tela anterior")

-- | Retorna uma String representativa da tela de jogo
gameScreen :: String
gameScreen = baseScreen
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
