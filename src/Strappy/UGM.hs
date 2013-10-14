
module Strappy.UGM where

import Strappy.Utils

import qualified Data.Set as Set

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
        headV = head vs2
        tailV = last vs1
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
ugmLatentify (UGM [Visible x] _) = UGM [Latent x] []

ugmChain :: [UGM] -> UGM
ugmChain = foldl ugmUnionHeadTail nullUGM

ugmRing :: [UGM] -> UGM
ugmRing vs = ugmUnionHeadTail (foldr1 ugmUnion vs) (ugmChain vs)

ugmIsing :: [[UGM]] -> UGM
ugmIsing vss = foldr1 ugmUnionCorrespondence (map ugmChain vss)

ugmHMM :: [UGM] -> UGM
ugmHMM vs = ugmUnionCorrespondence (foldl1 ugmUnion vs) (ugmChain $ map ugmLatentify vs)

ugmMRF :: [[UGM]] -> UGM
ugmMRF vss = ugmUnionCorrespondence (foldr1 ugmUnion $ concat vss) $
             ugmIsing $ map (map ugmLatentify) vss

isingRow start end = [ singletonUGM (Visible x) | x <- [start..end] ]
isingWorld = [ isingRow (start*3-2) (start*3) | start <- [1..3] ]