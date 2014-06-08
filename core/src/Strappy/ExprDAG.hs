module Strappy.ExprDAG where
        
import Strappy.Expr
import Strappy.Utils
import Strappy.Type


import Data.List
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set



data ExprNode = ExprLeaf Expr
              | ExprBranch Int Int
              deriving(Eq,Ord)
                
data ExprDAG = ExprDAG { dID2Node :: Map.Map Int ExprNode,
                         dNode2ID :: Map.Map ExprNode Int }

type DAGM = State ExprDAG

insertNode2DAG :: ExprNode -> DAGM Int
insertNode2DAG n = do 
  s@(ExprDAG {dID2Node = i2n, dNode2ID = n2i}) <- get
  case Map.lookup n n2i of
    Nothing -> let i = Map.size i2n
               in do put $ s { dID2Node = Map.insert i n i2n,
                               dNode2ID = Map.insert n i n2i }
                     return i
    Just i -> return i

loadExpr2DAG :: Expr -> DAGM Int
loadExpr2DAG e@(Term {}) = insertNode2DAG (ExprLeaf e)
loadExpr2DAG (App {eLeft = l, eRight = r}) = do 
  l' <- loadExpr2DAG l
  r' <- loadExpr2DAG r
  insertNode2DAG (ExprBranch l' r')
  
extractExprFromDAG :: Int -> DAGM Expr
extractExprFromDAG i = do 
  i2n <- liftM dID2Node get
  return (extract i2n i)
  where extract i2n j = 
          case Map.lookup j i2n of
            Nothing -> error "could not extract ID from DAG"
            Just (ExprLeaf e) -> e
            Just (ExprBranch c c') -> 
              extract i2n c <> extract i2n c'

buildTypeMap :: DAGM (Map.Map Int Type)
buildTypeMap = do 
  i2n <- liftM dID2Node get
  let numberPrograms = Map.size i2n
  tmap <- forM [0..numberPrograms-1] $ \ id -> 
    extractExprFromDAG id >>= \ e -> return (id,doTypeInference e)
  return $ Map.fromList tmap
  
dag2mdl :: ExprDAG -> 
           Set.Set Int ->  -- candidates for library membership
           Map.Map Int Type ->  -- map from ID to type
           Int ->  -- ID of the expression we're interested in
           Type ->  -- requested type
           [Int] ->  -- expressions in library
           Double -- minimum description length
dag2mdl s@(ExprDAG {dID2Node = i2n, dNode2ID = n2i})
  candidates id2type id requestedType = 
  let alternatives = filter (`Set.member` candidates) $ 
                     map fst $ 
                     filter (canUnifyFast requestedType . snd) $ 
                     Map.toList id2type
  in
   case Map.lookup id i2n of  
    Nothing -> error "dag2mdl: candidate not found"    
    Just (ExprBranch l r) -> 
      -- calculate the requested types for the argument and function
      let (leftRequested, rightRequested) = fromJust $ runTI $ do 
            alpha <- mkTVar
            requested' <- instantiateType requestedType
            leftType <- instantiateType $ safeFromJust "no type for left ID" $ 
                        Map.lookup l id2type
            unify (alpha ->- requested') leftType
            rightRequest <- applySub alpha
            return (alpha ->- requested', rightRequest)
          -- recurse on children
          l' = dag2mdl s candidates id2type l leftRequested
          r' = dag2mdl s candidates id2type r rightRequested
      in if id `Set.member` candidates
         then \ library -> 
              if id `elem` library
              then let alternatives' = filter (`elem` library)
                                       alternatives
                   in log2+(log $ genericLength alternatives')
              else log2+l' library+r' library
         else \ library -> log2+l' library+r' library
    Just (ExprLeaf{}) -> 
      \ library -> 
       let alternatives' = filter (`elem` library)
                           alternatives
       in log2+(log $ genericLength alternatives')
