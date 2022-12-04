module Utils.IO (readFileLines, readFileData) where

readFileLines :: String -> IO [String]
readFileLines fname = do
  content <- readFile fname
  return $ lines content

readFileData :: Read a => String -> IO [a]
readFileData fname = fmap (map read) (readFileLines fname)