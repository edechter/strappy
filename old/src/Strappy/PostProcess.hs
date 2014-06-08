--PostProcess.hs

module Strappy.PostProcess where


import System.IO
import System.Directory 
import System.FilePath
import Data.Time
import Data.List.Split hiding (oneOf)
import Data.List (isInfixOf)
import Data.Either.Utils
import Debug.Trace

import Text.ParserCombinators.Parsec 
import Text.Parsec.Token 
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language (haskellDef)
import Control.Applicative ((<$>), (<*>), (*>), (<*))


import Strappy.Search
import Strappy.CL
import Strappy.Task
import Strappy.Type
import qualified Strappy.CombMap as CM
import Strappy.CombMap (CombMap)
import Strappy.Grammar
import Strappy.Experiment
import Strappy.StdLib
import Strappy.ParseCL


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


saveSearchData :: Show a 
               => String  -- ^ path to data dir
               -> Experiment 
               -> [SearchLogEntry] 
               -> a -- ^ Time
               -> IO ()
saveSearchData datadir exp searchData time = 
    do expDirName <- currentExpId exp
      
       let expPath = joinPath [datadir, expDirName]
           grammarFileName = joinPath [expPath, "grammars.csv"]
           numhitFileName = joinPath [expPath, "numhit.csv"]
           explanationsFileName = joinPath [expPath, "explanations.csv"]
           experimentDataFileName = joinPath [expPath, "experimentdata.txt"]
           timeFileName = joinPath [expPath, "time.txt"]
       createDirectoryIfMissing True expPath
       fgrammar <- openFile grammarFileName WriteMode
       fnumHit <- openFile numhitFileName WriteMode
       fexpl <- openFile explanationsFileName WriteMode
       fexpdata <- openFile experimentDataFileName WriteMode
       ftime <- openFile timeFileName WriteMode
       hPutStr fgrammar (showLibByIter $ getLibData $ searchData)
       hPutStr fnumHit (showNumHitByIter $ getNumHitByIter $ searchData)
       hPutStr fexpl (showExplanationsByIter $ getExplanationsByIter $ searchData)
       hPutStr fexpdata (show exp)
       hPutStr ftime (show time)
       hClose fgrammar
       hClose fnumHit
       hClose fexpl
       hClose fexpdata
       hClose ftime
       
getLibFromFile filename = do
  str <- readFile filename
  let rows = splitOn "\n" str
      combs = map head $ map (splitOn ",") rows
  return combs

getGrammarFromFile lib filename = do
  str <- readFile filename
  let rows = splitOn "\n" str
      splitrows' = map (splitOn ",") rows
      combstrs = map head $ splitrows'
      valstrs = map last $ splitrows'
      pairstrs = filter (not . (\x -> isInfixOf "nan" x || null x) . snd) $ zip combstrs valstrs
      combs = (trace $ show pairstrs) $ map (fromRight .  parseExpr lib) $ map fst pairstrs
      vals = map (read .  snd  ) pairstrs :: [Double]
      lib' = (CM.fromList $ zip combs vals)
      grammar = Grammar lib' 0
  return grammar

getExplanationsFromFile lib filename 
    = do  str <- readFile filename
          let (Right explStrTuples) = parse explFile "" str
          let combs = map (map (fromRight . (parseExpr lib))) $ map (map snd) explStrTuples
          return combs


explFile :: GenParser Char st [[(String, String)]]
explFile = 
    do result <- many line
       eof
       return result

line :: GenParser Char st [(String, String)]
line = 
    do oneOf "["
       result <- cells
       oneOf "]"
       eol                       -- end of line
       return result

cells :: GenParser Char st [(String, String)]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

cellContent :: GenParser Char st (String, String)
cellContent = do a <- oneOf "(" *> many (noneOf ",")
                 oneOf ","
                 b <- many (noneOf ",[]") -- <* oneOf ")"
                 return (a, init b)

remainingCells = (char ',' >> cells) <|> (return [])

eol :: GenParser Char st String
eol = string "\n \n" <|> string ""
                      



-- getExplanationsFromFile lib filename 
--     = do str <- readFile filename
--          rows <- splitOn "\n" str
--          return ()
  
       
