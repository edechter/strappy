-- CombSequitur

module CombSequitur where

import Control.Monad.State
import qualified Data.Map as Map
import CL


newtype Symbol = Symbol Int deriving (Eq, Ord)
data Node = SNode Symbol | CombNode Comb deriving (Eq, Ord)
data SymTree = Leaf Node | Node {left :: SymTree, right :: SymTree} deriving (Eq, Ord)
type Rule = (Symbol, SymTree)
type Grammar = [Rule]
type Index = Map.Map SymTree Int

instance Show Symbol where
    show (Symbol i) = "c" ++ show i

comb2SymTree c@(CLeaf _ _ _) = Leaf (CombNode c)
comb2SymTree c@(CApp l r _) = Node (comb2SymTree l) (comb2SymTree r)


data Sequitur a = Sequitur (Grammar -> Index -> (Grammar, Index, a))
runSequitur :: Grammar -> Index -> Sequitur a -> (Grammar, Index, a)
runSequitur g i m = f g i
                    where (Sequitur f) = m

instance Monad Sequitur where
    return a = Sequitur $ \g i -> (g, i, a)
    m >>= f = Sequitur $ \g i ->
              let (g', i', a) = runSequitur g i m 
              in runSequitur g' i' (f a) 

putGrammar :: Grammar -> Sequitur ()
putGrammar g = Sequitur $ \_ i -> (g, i, ())

newSymbol g = Symbol (length g + 1)

putIndex :: Index -> Sequitur ()
putIndex index = Sequitur $ \g _ -> (g, index, ())

getGrammar :: Sequitur Grammar
getGrammar = Sequitur $ \g i -> (g, i, g)

getIndex :: Sequitur Index
getIndex = Sequitur $ \g i -> (g, i, i)

compress :: SymTree -> Sequitur ()
compress s = do g <- getGrammar
                s' <- scan s
                let r = (newSymbol g, s')
                putGrammar (r:g)
                
scan :: SymTree -> Sequitur SymTree
scan s@(Node (Leaf n1) (Leaf n2)) = do index <- getIndex
                                       g <- getGrammar
                                       case s `Map.member` index of
                                           True -> do let r = (newSymbol g, s)
                                                      putGrammar (r:g)
                                                      
                                           False -> undefined

                                         






