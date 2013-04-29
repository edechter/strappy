
module Strappy.Experiments.CSG where

import qualified Data.HashMap as Map

import Debug.Trace

import Strappy.Type
import Strappy.Expr
import Strappy.Library
import Strappy.Sample

import Language.Mecha


----------------------------------------
-- Type primitives ---------------------
----------------------------------------
tSolid = TCon $ TyCon "Solid" (Kfun Star Star)

----------------------------------------
-- Solid geometry primitives -----------
----------------------------------------
cSphere = Term "sphere" (tDouble ->- tSolid) $ sphere 
          
cCone = Term "cone" (tDouble ->- tDouble ->- tDouble ->- tSolid) $ cone

cCube = Term "cube" (tDouble ->- tSolid) $ cube

cCylinder = Term "cylinder" (tDouble ->- tDouble ->- tSolid) $ cylinder

----------------------------------------
-- Transformations ---------------------
----------------------------------------

cMove = Term "move" (tDouble ->- tDouble ->- tDouble ->- tSolid ->- tSolid) 
        $ ((\x y z -> move (x, y, z)) :: Double -> Double -> Double -> Solid -> Solid)

cRotateX = Term "rotateX" (tDouble ->- tSolid ->- tSolid) $ (rotateX :: Double -> Solid -> Solid)

cRotateY = Term "rotateY" (tDouble ->- tSolid ->- tSolid) $ (rotateY :: Double -> Solid -> Solid)

cRotateZ = Term "rotateZ" (tDouble ->- tSolid ->- tSolid) $ (rotateZ :: Double -> Solid -> Solid)

cScale = Term "scale" (tDouble ->- tDouble ->- tDouble ->- tSolid ->- tSolid)  
         $ ((\x y z -> scale (z, y, z)) :: Double -> Double -> Double  -> Solid -> Solid)

cMoveX = Term "moveX"  (tDouble ->- tSolid ->- tSolid) $ (moveX :: Double -> Solid -> Solid)

cMoveY = Term "moveY" (tDouble ->- tSolid ->- tSolid) $ (moveY :: Double -> Solid -> Solid)

cMoveZ = Term "moveZ" (tDouble ->- tSolid ->- tSolid) $ (moveZ :: Double -> Solid -> Solid)

cScaleAll = Term "scaleAll"  (tDouble ->- tSolid ->- tSolid) 
            $ (scaleAll :: Double -> Solid -> Solid)
            
cScaleX= Term "scaleX"  (tDouble ->- tSolid ->- tSolid) $ (scaleX :: Double -> Solid -> Solid)

cScaleY= Term "scaleY"  (tDouble ->- tSolid ->- tSolid) $ (scaleY :: Double -> Solid -> Solid)

cScaleZ= Term "scaleZ"  (tDouble ->- tSolid ->- tSolid) $ (scaleZ :: Double -> Solid -> Solid)

cUnions = Term "unions" ((TAp tList tSolid) ->- tSolid) $ (unions:: [Solid] -> Solid)

----------------------------------------
-- Combinations ------------------------
----------------------------------------

cUnion = Term "union" (tSolid ->- tSolid ->- tSolid) 
              $ (union :: Solid -> Solid -> Solid)

cDifference = Term "difference" (tSolid ->- tSolid ->- tSolid) 
              $ (difference :: Solid -> Solid -> Solid)

cIntersection = Term "intersection" (tSolid ->- tSolid ->- tSolid) 
                $ (intersection :: Solid -> Solid -> Solid)

csgExprs = [toUExpr cI, toUExpr cS, toUExpr cB, toUExpr cC, toUExpr cBottom,
            toUExpr cPlus, toUExpr cTimes, toUExpr cMinus, toUExpr cMod, toUExpr cRem,
            toUExpr cCons, toUExpr cAppend,  toUExpr cMap, 
            toUExpr cEmpty, toUExpr cSingle,
            toUExpr cRep, toUExpr cFoldl,
            toUExpr cSphere,
            toUExpr cCone,
            toUExpr cCube,
            toUExpr cCylinder,
            toUExpr cMove,
            toUExpr cRotateX,
            toUExpr cRotateY,
            toUExpr cRotateZ,
            toUExpr cScaleAll,
            toUExpr cScaleX,
            toUExpr cScaleY,
            toUExpr cScaleZ,
            toUExpr cUnions,
            toUExpr cUnion,
            toUExpr cDifference,
            toUExpr cIntersection
           ] ++ (map toUExpr cDoubles)

csgExprDistr = Map.adjust (const (-6)) (toUExpr cBottom) 
                $ Map.fromList [(e, 1) | e <- csgExprs]

csgGrammar = Grammar 6 csgExprDistr

----------------------------------------
-- Sample an openSCAD file -------------
----------------------------------------
sampleOpenSCAD library tp = do (expr, i) <- sampleExpr library tp
                               let solid = (trace $ show expr) eval expr
                                   str = openSCAD solid
                               writeFile "solid.scad" str 

  



            









