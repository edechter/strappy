-- | Convenience functions for visualization of combinators using GraphViz. 
module Strappy.Visualize where

import Data.GraphViz
import Data.Graph.Inductive

import Strappy.StdLib
import Strappy.Routers
import Strappy.CL
import qualified Strappy.Expr as Expr
import Strappy.Type
import Strappy.ParseCL
import Strappy.CombMap (CombMap)

import Debug.Trace


instance Labellable Comb where
    toLabelValue CLeaf{cName=n} = toLabelValue n
    toLabelValue c = toLabelValue ""

combToGraph :: Comb -> Gr Comb String
combToGraph c = uncurry mkGraph $ combToGraph' 1 c

combToGraph' :: Int -> Comb -> ([LNode Comb], [LEdge String])
combToGraph' i c@(CLeaf{}) = ([(i, c)], [])
combToGraph' i c@(CApp{lComb = cl, rComb = cr}) = 
    let rootNode = (i, c)
        (leftGraphNodes, leftGraphEdges) = combToGraph' (2*i) cl
        (rightGraphNodes, rightGraphEdges) = combToGraph' (2*i + 1) cr
        leftEdge = (i, 2*i, "")
        rightEdge = (i, 2*i + 1, "")
        nodes = rootNode : leftGraphNodes ++ rightGraphNodes
        edges = [leftEdge, rightEdge] ++  rightGraphEdges ++ leftGraphEdges
    in (nodes, edges)
    

params :: GraphvizParams n Comb el () Comb
params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l],
                              globalAttributes=[GraphAttrs [ordering OutEdges]]}


                             
(Right x) = parseExpr stdlib' "S * I"
(Right y) = parseExpr stdlib' "S B"
gr = combToGraph x
gr' = addCombToGraph gr y


inGraph :: Comb -> Gr Comb a -> Maybe (LNode Comb)
inGraph c gr = case lookup c (map (\(x,y) -> (y, x) )  $ labNodes gr) of
                 (Just l) -> Just (l, c)
                 Nothing -> Nothing

addCombToGraph :: Gr Comb String -> Comb -> Gr Comb String
addCombToGraph gr c@(CLeaf{})  = case inGraph c gr of 
                                   Just (node, c') -> gr
                                   Nothing -> mkGraph  
                                              ((newNode, c):(labNodes gr)) 
                                              (labEdges gr)
    where newNode = head $ newNodes 1 gr
addCombToGraph gr c@(CApp{lComb = cl, rComb = cr}) = 
    case inGraph c gr of 
      Just (node, c') -> gr
      Nothing -> (trace $ show nodes) 
                 $ mkGraph nodes edges
      where withLeft = addCombToGraph gr cl
            withLeftRight = addCombToGraph withLeft cr
            (Just clNode) = inGraph cl withLeftRight
            (Just crNode) = inGraph cr withLeftRight
            leftEdge  = (fst rootNode, fst clNode, "")
            rightEdge = (fst rootNode, fst crNode, "")
            newNode = head $ newNodes 1 withLeftRight
            rootNode = (newNode, c)
            edges = labEdges withLeftRight  ++ [ leftEdge, rightEdge] 
            nodes = rootNode:(labNodes withLeftRight)

combsToGraph :: [Comb] -> Gr Comb String
combsToGraph cs = foldl (addCombToGraph) (mkGraph [] []) cs