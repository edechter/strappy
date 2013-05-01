

module Strappy.Experiments.CSG where

import qualified Data.HashMap as Map

import System.Process
import System.IO
import System.Directory
import System.Posix

import Control.Monad
import Control.Exception

import Foreign.Ptr
import System.Environment
import Data.Word
import Data.Array.Repa hiding ((++), map)
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Eval

import Debug.Trace

import Strappy.Type
import Strappy.Expr
import Strappy.Library
import Strappy.Sample

import Language.Mecha


----------------------------------------
-- Type primitives ---------------------
----------------------------------------
tSolid = TCon $ TyCon "Solid" Star

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

csgGrammar = Grammar 4 csgExprDistr

----------------------------------------
-- Sample an openSCAD file -------------
----------------------------------------
sampleOpenSCAD :: String -> String -> Grammar -> IO FilePath
-- | Sample solid from <library> and write scad file to <filename>.
sampleOpenSCAD tempdir template library 
    = do (expr, i) <- sampleExpr library tSolid
         result <- try $ evaluate (eval expr)
         case result of
           Left error -> throw (error :: IOException)
           Right solid -> do let str = openSCAD solid
                             createDirectoryIfMissing True tempdir
                             (filepath, h) <- openTempFile tempdir  (template ++ ".scad")
                             hClose h
                             writeFile filepath str 
                             return filepath



sampleSolidPNG :: String -> String -> Grammar -> IO FilePath
sampleSolidPNG tempdir template library 
    = do scadPath <- sampleOpenSCAD tempdir template library
         (pngPath, h) <- openTempFile tempdir (template ++ ".png")
         hClose h
         setFileMode pngPath stdFileMode
         let cmd = "OpenSCAD" ++ " -o " ++ pngPath ++ " " ++ scadPath 
         putStrLn $ cmd
         system cmd
         return pngPath

sampleSolidImage :: String -> String -> Grammar -> IO (Array F DIM3 Word8)
sampleSolidImage tempdir template library
    = do pngPath <- sampleSolidPNG tempdir template library
         (RGB v) <- runIL $ readImage pngPath
         return v
         
                  
----------------------------------------
-- Sample a povray file ----------------
----------------------------------------
samplePovray :: String -> Grammar -> IO ()
-- | Sample solid from <library> and write scad file to <filename>.
samplePovray filename library = do (expr, i) <- sampleExpr library tSolid
                                   let solid = eval expr
                                       str = povray solid
                                   writeFile filename str 


----------------------------------------
-- 
  



            









