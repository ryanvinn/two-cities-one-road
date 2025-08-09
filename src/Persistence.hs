module Persistence where

import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (writeFile, readFile)
import Control.Monad (unless)

-- | Verifica e cria os arquivos do programa
verifyAndCreateFiles :: IO ()
verifyAndCreateFiles = do
  createDirectoryIfMissing True "data"

  exists1 <- doesFileExist file1
  exists2 <- doesFileExist file2

  unless exists1 (writeFile file1 "0")
  unless exists2 (writeFile file2 (unlines [
    "CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPC",
    "TODO",
    "TODO",
    "TODO",
    "TODO",
    "TODO",
    "TODO",
    "TODO",
    "TODO",
    "TODO"
    ]))

  where
    file1 = "data/next_map.txt"
    file2 = "data/maps.txt"

-- | Salva uma String em um arquivo
writeString :: FilePath -> String -> IO ()
writeString path content = writeFile path content

-- | Lê uma String de um arquivo
readString :: FilePath -> IO String
readString path = readFile path
