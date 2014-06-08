
module Strappy.Experiments.Words.Corpus where

import Data.Char


tokenize :: String -> [String]
-- | basic tokenizer: removes punctuation except for apostrophes and makes everything lowercase
tokenize txt = let ls = lines txt
                   clean = map (\x -> if x `elem` "!.,-\"\'!@#$%^&*()_+=?<>{}[];/" then ' ' else x) 
               in (concatMap words . map (map toLower) . map clean) ls

--main = do txt <- readFile "cedrick.txt"
--          putStrLn $ show (tokenize txt)
