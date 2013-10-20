
--module Strappy.UGM where
module Main where


import Strappy.EM
import Strappy.Type
import Strappy.Expr
import Strappy.Utils
import Strappy.Library
import Strappy.BeamSearch
import Strappy.Planner

import Data.Maybe
import qualified Data.Map as Map
import Data.List
import System.Environment
import System.Random
import Control.Monad


data Vertex = Visible Int | Latent Int
    deriving(Eq, Show)

instance Ord Vertex where
    compare (Visible x) (Visible y) = compare x y
    compare (Latent x) (Latent y) = compare x y
    compare (Visible {}) (Latent {}) = LT
    compare (Latent {}) (Visible {}) = GT

isLatent :: Vertex -> Bool
isLatent (Latent {}) = True
isLatent _ = False

isVisible :: Vertex -> Bool
isVisible = not . isLatent

getVertexID :: Vertex -> Int
getVertexID (Visible x) = x
getVertexID (Latent x) = x

-- | Undirected edge
-- IMPORTANT: Never make using constructor!
-- Use mkUEdge instead.
-- This is because we need to assume that the edges are unordered,
-- so each UEdge is in a "normal form",
-- where its first vertex is smaller than its second vertex
data UEdge = UEdge Vertex Vertex
    deriving(Eq, Ord, Show)
mkUEdge :: Vertex -> Vertex -> UEdge
mkUEdge v1 v2 | v1 < v2 = UEdge v1 v2
mkUEdge v1 v2 = UEdge v2 v1

data UGM = UGM [Vertex] [UEdge]
    deriving(Eq, Ord, Show)

nullUGM :: UGM
nullUGM = UGM [] []

singletonUGM :: Vertex -> UGM
singletonUGM v = UGM [v] []

-- | UGM combinators

ugmUnion :: UGM -> UGM -> UGM
ugmUnion (UGM vs es) (UGM vs' es') =
    UGM (nub $ vs ++ vs') (nub $ es ++ es')

-- | Preserves uniqueness of latent variables
ugmUnionDisjoint :: UGM -> UGM -> UGM
ugmUnionDisjoint ugm ugm' =
    let (ugm1, ugm2) = ugmRename2Disjoint ugm ugm'
    in ugmUnion ugm1 ugm2

-- | Connects nth vertex of UGM1 to nth vertex of UGM2
ugmUnionCorrespondence :: UGM -> UGM -> UGM
ugmUnionCorrespondence ugm ugm' =
    let (UGM vs1 es1, UGM vs2 es2) = ugmRename2Disjoint ugm ugm'
        newEdges = zipWith mkUEdge (sort vs1) (sort vs2)
    in UGM (nub $ vs1++vs2) (nub $ es1 ++ es2 ++ newEdges)

-- | Connects first vertex of a UGM to the last vertex of another UGM
ugmUnionHeadTail :: UGM -> UGM -> UGM
ugmUnionHeadTail ugm ugm' =
    let (UGM vs1 es1, UGM vs2 es2) = ugmRename2Disjoint ugm ugm'
        headV = minimum vs2
        tailV = maximum vs1
        newEdge = if null vs1 || null vs2 || headV == tailV
                  then []
                  else [mkUEdge headV tailV]
    in UGM (nub $ vs1 ++ vs2) (nub $ newEdge ++ es1 ++ es2)

ugmRename2Disjoint :: UGM -> UGM -> (UGM, UGM)
ugmRename2Disjoint u@(UGM [] _) u' = (u, u')
ugmRename2Disjoint u u'@(UGM [] _) = (u, u')
ugmRename2Disjoint u1@(UGM vs1 es1) u2@(UGM vs2 es2) =
    let -- Rename the latent variables in vs2 that collide with latent variables in vs1
        buildSub :: [Int] -> -- ^ List of latent variables to (possibly) rename
                    UGM -> -- ^ UGM to rename
                    UGM -- ^ New UGM
        buildSub [] u = u
        buildSub (l:ls) (UGM vs es) | (Latent l) `elem` vs1 = -- Collision
            let l' = head $ dropWhile (\newL -> (Latent newL) `elem` (vs1++vs)) [(l+1)..]
                renameVertex (Latent x) | x == l = Latent l'
                renameVertex x = x
                renameEdge (UEdge x y) = mkUEdge (renameVertex x) (renameVertex y)
                vs' = map renameVertex vs
                es' = map renameEdge es
            in buildSub ls (UGM vs' es')
        buildSub (l:ls) u = buildSub ls u
    in
        (u1, buildSub (map getVertexID $ filter isLatent vs2) u2)


-- | Test cases
ugmLatentify :: UGM -> UGM
ugmLatentify (UGM vs es) =
    let l (Visible x) = Latent x
        l x = x
        e (UEdge v v') = mkUEdge (l v) (l v')
    in UGM (nub $ map l vs) (nub $ map e es)

ugmChain :: [UGM] -> UGM
ugmChain = foldl1 ugmUnionHeadTail

ugmRing :: [UGM] -> UGM
ugmRing vs = ugmUnionHeadTail (foldr1 ugmUnion vs) (ugmChain vs)

ugmIsing :: [[UGM]] -> UGM
ugmIsing vss = foldr1 ugmUnionCorrespondence (map ugmChain vss)

ugmHMM :: [UGM] -> UGM
ugmHMM vs = ugmUnionCorrespondence (foldl1 ugmUnion vs) (ugmChain $ map ugmLatentify vs)

ugmMRF :: [[UGM]] -> UGM
ugmMRF vss = ugmUnionCorrespondence (foldr1 ugmUnion $ concat vss) $
             ugmIsing $ map (map ugmLatentify) vss

ugmLatentCommon :: [UGM] -> UGM
ugmLatentCommon = foldl ugmUnionHeadTail (singletonUGM $ Latent 1)

isingRow start end = [ singletonUGM (Visible x) | x <- [start..end] ]
isingWorld = [ isingRow (start*4-3) (start*4) | start <- [1..4] ]


{-makeUGMTask :: Eq b, Show b =>
               String -> -- ^ Name
               Type -> -- ^ Desired type
               [(a, b)] -> -- ^ Test cases
               EMTask-}
makeUGMTask nm tp tests =
    EMTask { etName = nm,
             etType = tp,
             etLogLikelihood = \e -> 
                let results = map (\(a, _) -> timeLimitedEval $ e <> (mkTerm undefined undefined a)) tests
                in if all isJust results
                   then let numHits = length $ filter (\(Just x, (_,y)) -> x == y) $ zip results tests
                            fractionHit = fromIntegral numHits / genericLength tests
                        in log fractionHit
                   else log 0.0
            }

-- | Combinators for UGM
tUGM = tGnd "UGM"
cUnion = mkTerm "union" (tUGM ->- tUGM ->- tUGM) $ ugmUnion
cUnionC = mkTerm "unionC" (tUGM ->- tUGM ->- tUGM) $ ugmUnionCorrespondence
cUnionHT = mkTerm "unionHT" (tUGM ->- tUGM ->- tUGM) $ ugmUnionHeadTail
cUnionD = mkTerm "unionD" (tUGM ->- tUGM ->- tUGM) $ ugmUnionDisjoint
cLatentify = mkTerm "latentify" (tUGM ->- tUGM) $ ugmLatentify
cLatent = mkTerm "latent" tUGM $ singletonUGM $ Latent 1
cNull = mkTerm "null" tUGM $ nullUGM

-- | Initial combinators
ugmLib = [cI, 
          cS, 
          cB,
          cC,
          cK,
          cFoldl,
          cFoldr,
          cConcat,
          cMap,
          cSingle,
          cUnion, cUnionC, cUnionD, cUnionHT, cLatentify, cLatent, cNull ]

-- | UGM tasks
chainTask :: EMTask
chainTask = makeUGMTask "chain" (tList tUGM ->- tUGM)
            [(isingRow 1 3, ugmChain (isingRow 1 3)),
             (isingRow 2 6, ugmChain (isingRow 2 6))]
ringTask :: EMTask
ringTask = makeUGMTask "ring" (tList tUGM ->- tUGM)
            [(isingRow 1 3, ugmRing (isingRow 1 3)),
             (isingRow 2 6, ugmRing (isingRow 2 6))]
isingTask :: EMTask
isingTask = makeUGMTask "ising" ((tList (tList tUGM)) ->- tUGM)
            [(isingWorld, ugmIsing isingWorld)]
hmmTask :: EMTask
hmmTask = makeUGMTask "hmm" (tList tUGM ->- tUGM)
            [(isingRow 1 1, ugmHMM (isingRow 1 1)),
             (isingRow 1 3, ugmHMM (isingRow 1 3)),
             (isingRow 2 6, ugmHMM (isingRow 2 6))]
latentTask :: EMTask
latentTask = makeUGMTask "latent" (tList tUGM ->- tUGM)
             [(isingRow 1 1, ugmHMM (isingRow 1 1))]
independentTask :: EMTask
independentTask = makeUGMTask "indep" (tList tUGM ->- tUGM)
              [(isingRow 1 4, foldl1 ugmUnion (isingRow 1 4))]
latentCommonTask :: EMTask
latentCommonTask = makeUGMTask "latentCommon" (tList tUGM ->- tUGM)
              [(isingRow 1 1, ugmLatentCommon (isingRow 1 1)),
               (isingRow 1 2, ugmLatentCommon (isingRow 1 2)),
               (isingRow 1 4, ugmLatentCommon (isingRow 1 4)),
               (isingRow 1 5, ugmLatentCommon (isingRow 1 5))]

main = do
  [rndSeed, planOrEM, lambda, pseudocounts, fSize, beamSize, planLen, prefix] <- getArgs
  setStdGen $ mkStdGen $ read rndSeed
  let planning = head planOrEM == 'p'
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- ugmLib ] }
  let tasks = [ independentTask, latentTask, latentCommonTask, chainTask, ringTask, isingTask, hmmTask ]
  let planTasks = map convert2planTask tasks
  loopM seed [0..14] $ \grammar step -> do
    if planning
       then putStrLn $ "EM Planning Iteration: " ++ show step
       else putStrLn $ "EM Iteration: " ++ show step
    grammar' <- if planning
                then liftM fst $ doEMBeam Nothing planTasks (read lambda) (read pseudocounts)
                                          (read fSize) (read beamSize) (read planLen) grammar
                else doEMIter (prefix++"/best_"++show step) tasks
                              (read lambda) (read pseudocounts) (read fSize) grammar
    saveGrammar (prefix++"/grammar_" ++ show step) grammar'
    return grammar'
  return ()