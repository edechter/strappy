-- Search.hs
-- Eyal Dechter

-- | This module contains functions for searching over CL libraries
-- given a dataset.

module Search where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.List.Extras.Argmax (argmaxWithMaxBy, argmax)
import Data.List (sortBy, foldl')
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Debug.Trace

import Type
import CL
import Expr
import CLError
import Enumerate
import StdLib
import Similarity
import Task
import Data
import ParseCL
import qualified CombTrie as CT
import qualified Compress as CP (getUniqueTrees)
import Compress (Index)
import Experiment

data SearchLogEntry = SearchLogEntry { searchIter :: Int,
                                       searchLib :: CT.CombTrie Int,
                                       searchSpace :: [Comb],
                                       searchDataSet :: DataSet,
                                       searchNumHit :: Int,
                                       searchExplanation :: [(Datum, Comb)],
                                       searchCompression :: CT.CombTrie Int
                                     } deriving Show
mkEmptySearchLog :: SearchLogEntry
mkEmptySearchLog  = SearchLogEntry 0 CT.empty [] [] 0 [] CT.empty

type Search a = State SearchLogEntry a 


runSearch :: Search a -> (a, SearchLogEntry)
runSearch s = runState s mkEmptySearchLog 

-- | Sort data by number of combinators matching each
sortData :: [(Task, [Comb])] -> [(Task, [Comb])]
sortData xs = sortBy 
              (\x1 x2 -> (length . snd $ x1) `compare` (length . snd $ x2))
              xs

-- | Depth first search
dfs :: [(Task, [Comb])] -> (Index, [(Task, Comb)])
dfs xs = argmax smallestNegIndex (g xs' (CT.empty, []))
    where xs' = sortData xs
          g :: [(Task, [Comb])] -> (Index, [(Task, Comb)]) -> [(Index, [(Task, Comb)])] 
          g ((d, cs):rest) (index, asc) = join $ map (g rest) vs 
              where vs = [(index `with` CP.getUniqueTrees c, (d, c):asc) | c <- cs]
                    with = CT.mergeWith (+)
          g [] x = [x]
          smallestNegIndex = ( (* (-1)) . length . CT.keys . fst )


 
-- | Greedy 
greedy :: [(Task, [Comb])] -> (Index, [(Task, Comb)])
greedy xs = foldl' g (CT.empty, []) xs
    where g (index, asc) (d, cs) = (index', (d, c'):asc)
              where with = CT.mergeWith (+)
                    vs = [(index `with` (CP.getUniqueTrees  c), c) | c <- cs]
                    (index', c') = argmax ( (* (-1)) . length . CT.keys . fst ) vs

-- | GreedyN 
greedyN :: Int -> [(Task, [Comb])] -> (Index, [(Task, Comb)])
greedyN n xs = let xss = take n (List.permutations xs)
                   out = map greedy xss
               in argmax ((* (-1)) . length . CT.keys . fst) out
                   
-- | For each data point in a dataset list all the expressions that
-- evaluate to that datapoint. 
findCombinatorsForEachDatum :: Experiment 
                            -> CT.CombTrie Int -- ^ current lib
                            -> Search [(Task, [Comb])]
findCombinatorsForEachDatum ex lib 
    = (trace $ "Total # of combinators: " 
                 ++ show (length cs)) 
      $ do s <- get
           put $ s{searchTaskSet=taskSet}
           return $ [(t, [ c | c <- cs, f c <= eps ]) | t@(Task n f) <- taskSet]
      where cs = map (fst . runTI) $ enumCombsToProb lib ll maxDepth tp
            -- vars
            taskSet = expTaskSet ex

            eps = expEps ex
            ll = expLogLikeBound ex
            maxDepth = expDepthBound ex
            tp = expDataType ex
            
            
-- -- | Choose best combinator for each datum in a greedy fashion. Remove
-- -- datums that don't have valid combinators. Sort datums by by number
-- -- of valid combinators. Successively choose combinator for each datum
-- -- such that the total number of unique subcombinators at each step is
-- -- minimized.
-- chooseCombinatorForEachDatumGreedy :: [(Datum, [Comb])]
--                                    -> ([(Datum, Comb)], CT.CombTrie Int)
-- chooseCombinatorForEachDatumGreedy xs
--     =  (out, uniques)
--       where xs' = List.sortBy ord $ filter ((not . null) . snd) xs
--                   where ord (_, xs) (_, ys) = compare (length xs) (length ys)
                                              
--             -- | returns (a, b) where a is best combinator, b is addition subcombs
--             h combTrie c = CT.mergeWith (+) combTrie $ countSubcombinators c
                                  
--             f combTrie cs = argmaxWithMaxBy 
--                                       (\x y -> compare 
--                                                (negate . CT.length $ x) 
--                                                (negate . CT.length $ y))
--                                       (h combTrie) cs

--             g :: (Datum, [Comb]) -> State (CT.CombTrie Int )(Datum, Comb)
--             g (d, cs) = do
--               acc <- get
--               let (c, acc') = f acc cs
--               put acc'
--               return $ (d, c)

--             (out, uniques) =  runState (sequence $ map g xs') CT.empty 

-- -- | Given a CombTrie of integers that count the number of times each
-- -- subcombinator appears, find the combinator that is greatest on the
-- -- score of depth times frequency.
-- chooseBestSubcombinator :: CT.CombTrie Int -> Comb
-- chooseBestSubcombinator ct = c
--     where cs = zip (CT.keys ct) $ 
--                zipWith (\x y -> ((combDepth x) * (y-1))) (CT.keys ct) (CT.toList ct)
             
--           best = (trace $ show (map snd cs)) $ List.maximumBy (\x y -> (snd x) `compare` (snd y)) cs
--           c = fst best

-- countSubTreesInComb :: CT.CombTrie a  -- ^ library
--                     -> Comb -- ^ combinator
--                     -> CT.CombTrie Int -- ^ new library with counts in combinator
-- countSubTreesInComb lib c@(CApp c1 c2 []) = 
--     case CT.lookup lib c of 
--       Nothing -> CT.mergeWith (+) l r 
--           where
--             l = countSubTreesInComb lib c1
--             r = countSubTreesInComb lib c2
--       Just _ -> CT.single c 1
-- countSubTreesInComb lib c = CT.single c 1 

-- countSubTrees :: CT.CombTrie Int -- ^ library
--               -> [Comb] -- ^ list of combs
--               -> CT.CombTrie Int
-- countSubTrees lib xs = foldl1 (CT.mergeWith (+)) (map countSubcombinators xs)

-- costOfSubTreeCounts :: CT.CombTrie Int -- ^ Library with counts
--                     -> CT.CombTrie Int -- ^ Subtree counts
--                     -> Double
-- costOfSubTreeCounts lib ct = sum $ map cost $ CT.keys ct
--     where 
--       cs = CT.keys ct
--       cost :: Comb -> Double
--       cost c = (-1) * (log $ fromIntegral num) - (log $ fromIntegral denom)
--                where others = filter (doesUnify c ) $ CT.keys lib
--                      num = fromJust $ CT.lookup ct c
--                      denom = sum $ map (fromJust . (CT.lookup lib)) others
--                      doesUnify c1 c2 = runTI $ do t1' <-  freshInstComb c1 >>= typeCheck
--                                                   t2' <-  freshInstComb c2 >>= typeCheck
--                                                   unify' t1' t2'
                                          
-- chooseBestCombinatorsForDatumsOnCost :: CT.CombTrie Int 
--                                      -> [(Datum, [Comb])]
--                                      -> [(Datum, Comb)]
-- chooseBestCombinatorsForDatumsOnCost lib xs = bests
--     where
--       xs' = filter ((not . null) . snd) xs
--       chooseBest (c:cs) = f cs (cost c) c
--           where cost x = costOfSubTreeCounts lib $ countSubTreesInComb lib x
--                 f (x:xs) m best =  if cost x < m then x
--                                   else best
--                 f [] m best = best
                
--       bests = [ (d, chooseBest cs) | (d, cs) <- xs']


-- showL (x:xs) = show x ++ "\n" ++ showL xs
-- showL [] = ""

-- oneStep :: Experiment 
--         -> CT.CombTrie Int -- ^ current lib
--         -> CT.CombTrie Int
-- oneStep ex lib = CT.mergeWith (+) out prior
--     where y = findCombinatorsForEachDatum ex lib
          
--           w =  (trace $ "Number of hit examples: " ++
--                       show (length $ filter ( not . null . snd) y)
--                 ++ "/" ++ show (length y)
--                )
--  --              chooseBestCombinatorsForDatumsOnCost lib y
--                chooseCombinatorForEachDatumGreedy y
--           best = chooseBestSubcombinator $ snd w
--           bestCount = fromJust $ CT.lookup (snd w) best
-- --          counts = countSubTrees lib (map snd w)
-- --          best = chooseBestSubcombinator counts
-- --          bestCount = fromJust $ CT.lookup counts best
--           lib' = (trace $ "Best  : " ++ show best
--                         ++ "\n --> lib:\n " ++ (unlines $ map (\(a,b) -> show' a ++ ":    " ++ show b) (CT.toAscList lib))
-- --                               ++ "\n --> inserting : " ++ show best
--                                ++ "\n --> with key : " ++ show bestCount
--                                ++ "\n " ++ (unlines $ map (\(a,b) -> show' b ++ ":    " ++ show a) (fst w))
--                  ) 
--                  $ CT.insert best bestCount lib
--           goodCombs = [c | (d, c) <- (fst w)]
-- --          goodCombs = [c | (d, c) <- w]
--           lib'' = foldl1 (CT.mergeWith (+)) (map (countSubTreesInComb lib') (goodCombs)) 
                  
--           out = giveNamesTo lib''
--           giveNamesTo cs = CT.fromList $ zip cs' vals
--               where cs' = [ if cName c == "" then c {cName = "c"} else c | c <- CT.keys cs]
--                     vals = CT.toList cs
--           -- vars
--           ds = expDataSet ex
--           eval = expEval ex
--           eps = expEps ex
--           ll = expLogLikeBound ex
--           maxDepth = expDepthBound ex
--           tp = expDataType ex
--           prior = expPrior ex

                           

-- loop :: Experiment -> CT.CombTrie Int
-- loop ex
--     = foldl (\l _ -> oneStep ex l) lib [0..reps]
--       where           
--         -- vars
--         ds = expDataSet ex
--         eval = expEval ex
--         eps = expEps ex
--         ll = expLogLikeBound ex
--         maxDepth = expDepthBound ex
--         tp = expDataType ex
--         prior = expPrior ex
--         lib = expInitLib ex
--         reps = expReps ex







