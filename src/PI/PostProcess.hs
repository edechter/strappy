--PostProcess.hs

module PostProcess where

import System.IO
import System.Directory 
import System.FilePath
import Data.Time
import qualified Data.Text as T

import Search
import CL
import Task
import Type
import qualified CombMap as CM
import CombMap (CombMap)
import Grammar
import Experiment

allCombs :: [SearchLogEntry] -> [Comb]
allCombs ss = CM.keys $ foldl1 CM.union $ map (library . searchGrammar) ss 

getLibData :: [SearchLogEntry] -> [(Comb, [Maybe Double])]
getLibData ss = let combs = allCombs ss
            in [(c, getDataForComb c ss) | c <- combs]

getDataForComb :: Comb -> [SearchLogEntry] -> [Maybe Double]
getDataForComb c (s:ss) = (CM.lookup c lib):(getDataForComb c ss)
                          where lib = (library . searchGrammar) s
getDataForComb c [] = []
           

showLibByIter :: [(Comb, [Maybe Double])] -> String
showLibByIter cs  = unlines ls
    where ls :: [String]
          ls = map (\(k, v) -> 
                        show k ++ ", " ++ (toCSV v)) cs
          toEntry :: Maybe Double -> String
          toEntry Nothing = "nan"
          toEntry (Just x) = show x
          toCSV :: [Maybe Double] -> String
          toCSV xs = foldl1 (\a b -> a ++ ", " ++ b) (map toEntry xs)

getNumHitByIter :: [SearchLogEntry] -> [Int]
getNumHitByIter ss = map (length . searchExplanation) ss

getExplanationsByIter :: [SearchLogEntry] -> [[(Task, Comb)]]
getExplanationsByIter ss = map (searchExplanation) ss

showNumHitByIter xs =  (foldl1 (\a b -> a ++ ", " ++ b) (map show xs))

showExplanation :: [(Task, Comb)] -> String
showExplanation xs = unlines $ map f xs
    where f (t, c) = show t ++ ", " ++ show' c
showExplanationsByIter xs =  (foldl1 (\a b -> a ++ "\n \n" ++ b) (map show xs))


spaceToUnderscore :: String -> String
spaceToUnderscore str = map (\x -> if x == ' ' then '_' else x) str
colonToDash str = map (\x -> if x == ':' then '-' else x) str
reformat = spaceToUnderscore . colonToDash

currentExpId :: Experiment -> IO String
currentExpId exp = do timeStr <- fmap show getZonedTime
                      let timeStr' = reformat timeStr
                          expId = (reformat $ expName exp) ++ "_" ++ timeStr'
                      return expId


saveSearchData :: String  -- ^ path to data dir
               -> Experiment 
               -> [SearchLogEntry] 
               -> IO ()
saveSearchData datadir exp searchData = 
    do expDirName <- currentExpId exp
      
       let expPath = joinPath [datadir, expDirName]
           grammarFileName = joinPath [expPath, "grammars.csv"]
           numhitFileName = joinPath [expPath, "numhit.csv"]
           explanationsFileName = joinPath [expPath, "explanations.csv"]
           experimentDataFileName = joinPath [expPath, "experimentdata.txt"]
       createDirectoryIfMissing True expPath
       fgrammar <- openFile grammarFileName WriteMode
       fnumHit <- openFile numhitFileName WriteMode
       fexpl <- openFile explanationsFileName WriteMode
       fexpdata <- openFile experimentDataFileName WriteMode
       hPutStr fgrammar (showLibByIter $ getLibData $ searchData)
       hPutStr fnumHit (showNumHitByIter $ getNumHitByIter $ searchData)
       hPutStr fexpl (showExplanationsByIter $ getExplanationsByIter $ searchData)
       hPutStr fexpdata (show exp)
       hClose fgrammar
       hClose fnumHit
       hClose fexpl
       hClose fexpdata
       
       
       
