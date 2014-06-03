module Main where

import Strappy.Config
import Strappy.Expr
import Strappy.Type
import Strappy.Utils
import Strappy.Library
import Strappy.BeamSearch
import Strappy.Planner
import Strappy.EM
import Strappy.ProdFeat
import Strappy.Sample
import Strappy.EnumBits
import Strappy.Response

import qualified Data.Map as Map
import System.Environment
import System.Random
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.List
import Data.Array

doE :: Type     -- ^ Type we're interested in
    -> Double   -- ^ pseudocounts
    -> Int      -- ^ frontier size
    -> Grammar  -- ^ grammar
    -> Maybe (Expr,Double)
doE exploredType pseudocounts frontierSize grammar =
  -- Sample frontiers (a Map from full expressions to their log likelihoods)
  let frontier = (\tp -> sampleBits frontierSize grammar tp) exploredType
      llMinimum = (minimumBy (\(_,ll1) (_,ll2) -> compare (ll1) (ll2))) 
  in  ((\front -> if (null front) then Nothing else (Just $ llMinimum front)) $ Map.toList frontier)

main = do
    args@[rndSeed, pseudocounts, prefix] <- getArgs
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.375,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberExprs ] }
    let exploredType = tTriple (tList tChar) (tList tChar) (tList tInt) ->- (tList tInt)
    let fSizes = [25, 50]++[100,200..900]++[1000,2000..10000]
    let results = map (\fSize -> let worst = doE exploredType (read pseudocounts) fSize seed
                                  in  maybe ((show fSize),"",(show 0)) (\(e,ll) -> (show fSize, show e, show ll)) worst)
                      fSizes
    let labeledResults = (\[x,y,z] (a,b,c) -> [(x,a),(y,b),(z,c)]) ["fSizes","exprs","lls"] (unzip3 results)
    let outStrings = map (\(n,xs) -> n ++ "= [" ++ (intercalate "," xs) ++ "];") $ labeledResults
    writeFile (prefix++"results.txt") $ unlines outStrings
    putStrLn $ "Finished!"
