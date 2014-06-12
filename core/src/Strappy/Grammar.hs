-- Grammar.hs
-- |
-- Module:      Strappy.Core.Grammar
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- This module defines data types and methods for distributions over
-- expressions. Distributions are specified as weighted grammars.

module Strappy.Grammar (
   -- * Grammar
  Grammar(..),
  normalizeGrammar,
  getUnifyingPrims,
  exprLogLikelihood,
  -- * ExprMap        
  ExprMap,
  showExprDistr,
     ) where

-- External imports --
import Data.Maybe 
import qualified Data.Map as Map hiding ((\\))
import Data.Set (Set())
import qualified Data.Set as Set
import Data.Hashable
import GHC.Prim
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.List as List
import Data.List ((\\))
import Text.Printf
import Data.Function (on)
import Control.Monad.Identity
import Control.Monad.State
import Control.Arrow (first)
import Debug.Trace
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Directory
import Data.String (IsString)
import Control.Monad.Error.Class

-- Strappy imports -- 
import Strappy.Type
import Strappy.Expr
import Numeric.StatsUtils


-- | Type alias for hash table mapping expressions to values. 
type ExprMap a = Map.Map Expr a

-- | Type alias for distribution over expressions. Values are
-- interpreted as normalized or unnormalized log probabilities.
type ExprDistr = ExprMap Double 

showExprDistr :: ExprDistr -> String
showExprDistr exprDistr  = unlines $ map (\(e, i) -> printf "%7s:%7.2f" (show e) i) pairs
    where pairs = List.sortBy (compare `on` snd) $ Map.toList exprDistr

-- | Type for stochastic grammar over programs. Note that, when
-- normalized, the @ExprDistr@ is interpreted as the distribution over
-- expressions conditioned on an application being requested.
data Grammar = Grammar {grApp :: Double,         -- ^ log probability of application
                        grExprDistr :: ExprDistr -- ^ distribution over functions
                       }

instance Show Grammar where
  show (Grammar p exprDistr) = printf "%7s:%7.2f\n" "p" (exp p) ++ showExprDistr exprDistr

-- | Normalizes both the application and expression distribution
-- fields of the grammar.
normalizeGrammar :: Grammar -> Grammar 
normalizeGrammar gr@Grammar{grExprDistr=distr} =
  let distr' = Map.fromList $ normalizeDist $ Map.toList distr
  in gr { grExprDistr = distr' }


-- | Return the primitives in the grammar that unify with a given
-- type. Expressions and their weights are returned with the current
-- type context.
getUnifyingPrims :: MonadError String m => Grammar -> Type -> [TypeInference m (Expr, Double)]
getUnifyingPrims (Grammar gamma exprDistr) tp = do
  (e, w) <- Map.toList exprDistr
  return $ do unifyExpr tp e
              return (e, w)

-- | Return the loglikelihood that the given expression would be
-- produced under the given grammar if an expresssion of the type
-- argument were requested. Assumes an empty context.
exprLogLikelihood :: MonadError String m => Grammar -> Type -> Expr -> m Double
exprLogLikelihood gr tp expr = evalTI $ logLikelihoodExprM gr tp expr

-- | Return the loglikelihood of producing the given expression. This
-- is the calculation defined in the Dechter et. al, IJCAI paper.
logLikelihoodExprM :: MonadError String m => Grammar -> Type -> Expr -> TypeInference m Double
logLikelihoodExprM gr@(Grammar gamma _) tp expr = do
  llPrim <- logLikelihoodPrimM gr tp expr
  llApp <- logLikelihoodAppM gr tp expr
  let logpNoApp = log (1 - exp gamma)
  return $ logSumExp ( logpNoApp + llPrim) (llApp + gamma)
  

-- | Return the loglikelihood of returning a given primitive from the
-- library given a current requested type, and conditioned on being
-- asked for a primitive.
logLikelihoodPrimM :: MonadError String m => Grammar -> Type -> Expr -> TypeInference m Double
logLikelihoodPrimM gr@(Grammar gamma exprDistr) tp expr = do
  (ctx :: Context) <- get
  let loop !acc [] = return $! acc
      loop !acc (x:xs) = do (_, w) <- x
                            loop (w:acc) xs
  w_alts <- lift $ loop [] [evalStateT m ctx | m <- getUnifyingPrims gr tp]
  let logZ = logSumExpList w_alts
  case Map.lookup expr exprDistr  of
    Nothing -> return $! negInfty
    Just ll  -> return $! ll - logZ

  
-- | Return the loglikelihood of returning a given expr from the
-- library given a current requested type, and conditioned on being
-- asked for an application.
logLikelihoodAppM :: MonadError String m => Grammar -> Type -> Expr -> TypeInference m Double
logLikelihoodAppM _ _ Term{} = return $! negInfty -- probability 0 of getting a terminal
logLikelihoodAppM gr@(Grammar gamma exprDistr) tp (App eL eR eTp) = do
  do eta <- mkTVar
     llL <- logLikelihoodExprM gr (eta ->- tp) eL
     llR <- logLikelihoodExprM gr eta eR
     return $ llL + llR

negInfty :: Double
negInfty = read "-Infinity"

