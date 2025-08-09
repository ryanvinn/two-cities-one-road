module Interfaces where

import Data.List (intercalate) -- Para intercalar dados na formatação
import Map (getGameHeader, getGameMatrix)

-- | Retorna uma String representativa da tela inicial
homeScreen :: String
homeScreen = baseScreen
  (middleJustifyLine "Two Cities One Road")
  (topJustifyColumn [
    "Bem vindo ao jogo!",
    "",
    "Seu objetivo é conectar duas cidades com dinheiro",
    "limitado e diversos obstáculos naturais.",
    "",
    "Durante o jogo, use as teclas de direção (w, a, s, d)",
    "para construir trilhos nos quadrantes vizinhos ao seu."
  ])
  (middleJustifyLine "A: acessar último mapa, G: gerar mapa, Q: sair")

-- | Retorna uma String representativa da tela de jogo
-- Para tanto, a partir do número do mapa, busca o arquivo recpectivo na memória
-- (com os valores da matriz 5x11 e os valores dos recursos)
gameScreen :: IO String
gameScreen = do
  header <- getGameHeader
  matrix <- getGameMatrix
  let topPart = leftJustifyLine header
      middlePart = middleJustifyColumn matrix
      bottomPart = middleJustifyLine "W: cima, A: esquerda, D: direita, S: baixo, Q: sair"
  return $ baseScreen topPart middlePart bottomPart

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

-- | Retorna o tamanho de uma coluna
column_length :: Int
column_length = 15

-- | Retorna o tamanho de uma linha
line_length :: Int
line_length = 55
