

module Strappy.Alternatives.GeneticProgramming where

-- We implement a typed genetic programming search over our expressions. 
-- The steps we need are 1) initialization of population, 2) selecting best
-- n% of the population for reproduction, 3) generation of a new population
-- via crossover and mutation. 
--
import Prelude hiding (flip)
import Control.Monad.Random
import Control.Monad

import Strappy.Core

data GPconfig = GPconfig {gpPopulatonSize :: Integer,
                          gpPercentMutation :: Double,
                          gpMaxMutationDepth :: Integer,
                          gpInitDepth :: Integer} deriving Show

initPopulation :: (MonadRandom m, MonadPlus m) =>
                [Expr] -- primitive set 
                -> Integer -- population size
                -> Type 
                -> m (ExprMap (Double, Sub)) -- population
-- | Initialize population 
initPopulation primitives populationSize tp 
    = sampleExprsWithContexts (fromIntegral populationSize) gr tp
    where gr = normalizeGrammar $ Grammar 3 (mkExprDistr primitives)
   

chooseRandomNode :: (MonadPlus m, MonadRandom m) => Expr -> m (Expr, Path)
chooseRandomNode e@App{eLeft=l, eRight=r} 
    = do shouldChooseItself <- flip (0.9 :: Double)
         if not shouldChooseItself   
            then do shouldChooseLeft <- flip (0.5 :: Double)
                    if shouldChooseLeft 
                      then do (l', path) <- chooseRandomNode l
                              return (l' <> r, L : path) 
                      else do (r', path) <- chooseRandomNode r
                              return (l <> r', R : path) 
            else return (cBottom, [])
chooseRandomNode e@Term{} = return (cBottom, [])

data Dir = L | R deriving (Show, Eq)
type Path =  [Dir] 
mutateExpr :: (MonadPlus m, MonadRandom m) 
            => [Expr] -- ^ primitive set
            -> Expr -- ^ expr to mutate
            -> m Expr
mutateExpr primitives expr = do (expr', path) <- chooseRandomNode expr
                                replaceEmptyNode expr' path 

      where 
            gr = normalizeGrammar $ Grammar 3 (mkExprDistr primitives)
    
            chooseRandomNode :: (MonadPlus m, MonadRandom m) => Expr -> m (Expr, Path)
            chooseRandomNode e@App{eLeft=l, eRight=r} 
                = do shouldChooseItself <- flip (0.9 :: Double)
                     if not shouldChooseItself   
                        then do shouldChooseLeft <- flip (0.5 :: Double)
                                if shouldChooseLeft 
                                  then do (l', path) <- chooseRandomNode l
                                          return (l' <> r, L : path) 
                                  else do (r', path) <- chooseRandomNode r
                                          return (l <> r', R : path) 
                        else return (cBottom, [])
            chooseRandomNode e@Term{} = return (cBottom, [])

            replaceEmptyNode :: (MonadPlus m, MonadRandom m) => Expr -> Path -> m Expr
            replaceEmptyNode e p = let emptyNode = index e p 
                                       tp = eType emptyNode
                                       ti = do typeCheck e
                                               tp' <- applySub tp
                                               sampleExprM gr tp'
                                       replacement = do out <- (evalTI ti)
                                                        return $ either (\_ -> error "" ) id out
                                       replaceNode [] expr repl = repl
                                       replaceNode (L:xs) expr@App{eLeft=l, eRight=r} repl = replaceNode xs l repl <> r
                                       replaceNode (R:xs) expr@App{eLeft=l, eRight=r} repl = l <> replaceNode xs r repl 
                                   in do r <- replacement 
                                         return $ replaceNode p e r

            index e@Term{} [] = e
            index e@App{eLeft=l, eRight=r} (L:xs)= l 
            index e@App{eLeft=l, eRight=r} (R:xs)= r   
            index _ _ = error "mutateExpr@index: Unable to index into expression with path."       

crossoverExprs :: (MonadRandom m, MonadPlus m)
            => [Expr] -- ^ primitive set
            -> Expr -- ^ parent 1
            -> Expr -- ^ parent 2
            -> m (Expr, Expr) -- ^ children with swapped subtrees, returns
                              -- mzero if no compatible subtrees
crossoverExprs = undefined


