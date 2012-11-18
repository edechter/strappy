-- NumberGame.hs
{-# Language ParallelListComp #-}
module Main where

import qualified Data.Map as Map
import Data.List.Extras (argmax)
import Control.Monad
import Data.Maybe

import CL
import StdLib (stdMultinomialLib, cI)
import Expr
import PCFG
import CLError
import Enumerate
import Debug.Trace

type DataSet = [Expr]

-- set variables --
maxDepth = 3
dataType = Rtype
reps = 2

----------------------------



oneStep :: DataSet 
        -> MultinomialLib
        -> Int -- ^ maxDepth
        -> Type
        -> Either CLError MultinomialLib
oneStep dataSet lib maxDepth tp
    = let combs = Map.keys lib
      in do let cs = (trace $ "one step!") $ map fst $ enumCombsToDepth combs maxDepth tp
                vals = map (reduce . comb2Expr') cs
--            vals <- return $ map reduce exprs
            
            -- lls <- mapM (\c -> fmap fst $ combLogLikelihood lib c tp) cs
--             let y  =  map fromJust 
--                       $ filter isJust [ getCombForDatum d cs vals lls | d <- dataSet]
--                 w = map fst y                
--                 fr =  countTreeFrequencies w
--                 fr' = Map.filterWithKey (\k v -> not $ any (eq' k) (Map.keys lib)) fr
--                 depths = Map.fromList $ [(c,  combDepth c) | c <- (Map.keys fr')]
--                 ranks  = Map.unionWith (*) fr' depths
--                 (best_c, num) = argmax snd (Map.toList $ ranks)
--                 goodCombs = combsThatHitData dataSet cs vals
--                 -- lib' = Map.insert best_c num lib
--                 relabeledCombs = relabelCombsSubtrees 
--                                  (map cName $ Map.keys lib) 
--                                  goodCombs best_c
                lib' = countLeafFrequencies cs -- relabeledCombs                 
            return lib'

combsThatHitData :: DataSet -> [Comb] -> [Expr] -> [Comb]
combsThatHitData dataSet cs vs
    = join [ [ cs !! i | (v, i) <- (zip vs [0..]), v == datum]| datum <- dataSet]

getCombForDatum :: Expr -> [Comb] -> [Expr] -> [Double] -> Maybe (Comb, Double)
getCombForDatum datum cs vs ls 
    = let xs = [ (cs !! i, ls !! i) | (v, i) <- (zip vs [0..]), v==datum]
      in if null xs then Nothing else Just $ argmax snd  xs

run = do
  let dats = [(R (2^i)) | i <- [1..10]]
  out <- foldM (\l _ -> oneStep dats l maxDepth dataType) stdMultinomialLib [0..10]
  return $ out

main = do
  let out = extractValue run
  putStrLn $ show out
            
            
            



