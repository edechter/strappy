-- Library.hs
-- |
-- Module:      Strappy.Core.Library
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | This module defines collections of standard primitives and types
-- for our expression language.


module Strappy.Library where

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
import Strappy.Grammar

-- Removes new non-terminals and puts a uniform distribution over the productions
blankLibrary :: Grammar -> Grammar
blankLibrary (Grammar {grExprDistr = distr}) =
  let leaves = filter isTerm $ Map.keys distr
  in Grammar { grExprDistr = Map.fromList [ (l, 0.0) | l <- leaves ],
               grApp = log 0.45}

---------------------------------------------------------------------

-- | Helper for turning a Haskell type to Any.
mkAny :: a -> Any
mkAny = unsafeCoerce  



-- | Basic combinators
cBottom :: Expr
cBottom = mkTerm "_|_" (TVar 0) (error "cBottom: this should never be called!") 

cI = mkTerm "I" (t ->- t) $ id

cS = mkTerm "S" ((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t) $
     \f g x -> f x (g x)

cB = mkTerm "B" ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t) $
     \f g x -> f (g x)

cC = mkTerm "C" ((t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t) $
     \f g x -> f x g

cK = mkTerm "K" (t1 ->- t2 ->- t1) $
     \x y -> x

cW = mkTerm "W" ((t1 ->- t1 ->- t) ->- t1 ->- t) $
    \x y -> x y y

cS' = mkTerm "S'" ((t3 ->- t2 ->- t1 ->- t) ->- (t3 ->- t2 ->- t1) ->- (t3 ->- t2) ->- t3 ->- t) $
     \f g h x -> (f x) (g x) (h x)

sFix :: (a -> a) -> a
sFix f = f (sFix f)

cFix = mkTerm "fix" ((t ->- t) ->- t) $
    \f -> sFix f

cSS = mkTerm "SS" ((t2 ->- t3 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t) $ \ f g x1 x2 -> (f x1 x2) (g x1 x2)

cSB = mkTerm "SB" ((t2 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t) $ \ f g x1 x2 -> (f x1) (g x1 x2)

cSC = mkTerm "SC" ((t3 ->- t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t3 ->- t1 ->- t) $ \ f g x1 x2 -> (f x1 x2) (g x1)

cBS = mkTerm "BS" ((t3 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t) $ \ f g x1 x2 -> (f x2) (g x1 x2)

cBB = mkTerm "BB" ((t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t) $ \ f g x1 x2 -> f (g x1 x2)

cBC = mkTerm "BC" ((t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t3 ->- t1 ->- t) $ \ f g x1 x2 -> (f x2) (g x1)

cCS = mkTerm "CS" ((t1 ->- t3 ->- t2 ->- t) ->- (t3 ->- t2) ->- t1 ->- t3 ->- t) $ \ f g x1 x2 -> (f x1 x2) (g x2)

cCB = mkTerm "CB" ((t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t1 ->- t3 ->- t) $ \ f g x1 x2 -> (f x1) (g x2)

cCC = mkTerm "CC" ((t1 ->- t2 ->- t3 ->- t) ->- t3 ->- t1 ->- t2 ->- t) $ \ f g x1 x2 -> (f x1 x2) (g)

cDualCombinators = [cSS,cSB,cSC,cBS,cBB,cBC,cCS,cCB,cCC]

-- | Holes
cHole :: Expr
cHole = mkTerm "H" tDouble $ error "Attempt to evaluate a hole"

-- | Tuples
cPair :: Expr
cPair = mkTerm "pair" (t1 ->- t2 ->- tPair t1 t2) (,)

cFst :: Expr
cFst = mkTerm "fst" (tPair t1 t2 ->- t1) fst

cSnd :: Expr
cSnd = mkTerm "snd" (tPair t1 t2 ->- t2) snd

cOnFst :: Expr
cOnFst = mkTerm "onFst" ((t1 ->- t2) ->- (tPair t1 t) ->- (tPair t2 t)) $
         \f (a,b) -> (f a, b)
cOnSnd :: Expr
cOnSnd = mkTerm "onSnd" ((t1 ->- t2) ->- (tPair t t1) ->- (tPair t t2)) $
         \f (a,b) -> (a, f b)

cSwap :: Expr
cSwap = mkTerm "Swap" ((tPair t1 t2) ->- (tPair t2 t1)) $ \(a,b) -> (b,a)

cTriple :: Expr
cTriple = mkTerm "triple" (t ->- t1 ->- t2 ->- tTriple t t1 t2) $ \ x y z -> (x,y,z)
c3Fst :: Expr
c3Fst = mkTerm "3fst" (tTriple t t1 t2 ->- t ) $ \(x,_,_) -> x
c3Snd :: Expr
c3Snd = mkTerm "3snd" (tTriple t t1 t2 ->- t1) $ \(_,x,_) -> x
c3Trd :: Expr
c3Trd = mkTerm "3trd" (tTriple t t1 t2 ->- t2) $ \(_,_,x) -> x

-- | Integer arithmetic
cPlus :: Expr
cPlus = mkTerm "+" (tInt ->- tInt ->- tInt) $
        (+)

cTimes :: Expr
cTimes = mkTerm "*" (tInt ->- tInt ->- tInt) $
         (*)

cMinus :: Expr
cMinus = mkTerm "-" (tInt ->- tInt ->- tInt) $
         (-)

-- | Floating-point arithmetic
cFPlus :: Expr
cFPlus = mkTerm "+." (tDouble ->- tDouble ->- tDouble) $
        ((+) :: Double -> Double -> Double)

cFDiv :: Expr
cFDiv = mkTerm "/." (tDouble ->- tDouble ->- tDouble) $
        ((/) :: Double -> Double -> Double)

cFTimes :: Expr
cFTimes = mkTerm "*." (tDouble ->- tDouble ->- tDouble) $
         ((*) :: Double -> Double -> Double)

cFMinus :: Expr
cFMinus = mkTerm "-." (tDouble ->- tDouble ->- tDouble) $
         ((-) :: Double -> Double -> Double)

-- | Lists
cCons = mkTerm ":"  (t ->- tList t ->- tList t) $
        (:)
cAppend = mkTerm "++" (tList t ->- tList t ->- tList t) $
          (++)
cHead = mkTerm "head" (tList t ->- t) $ 
        head
cTail = mkTerm "tail" (tList t ->- tList t) $ 
        tail
cMap = mkTerm "map" ((t ->- t1) ->- tList t ->- tList t1) $
       map
cFilter = mkTerm "filter" ((t ->- tBool) ->- tList t ->- tList t) $ filter
cEmpty = mkTerm "[]" (tList t) $ []
cSingle = mkTerm "single" (t ->- tList t) $ 
          \x -> [x]
cRep = mkTerm "rep" (tInt ->- t ->- tList t) $
       replicate
cConcat = mkTerm "concat" (tList (tList t) ->- tList t) concat
cReverse = mkTerm "reverse" (tList t ->- tList t) reverse
cFoldl = mkTerm "foldl" ((t ->- t1 ->- t) ->- t ->- tList t1 ->- t) $ 
         List.foldl'
cFoldr = mkTerm "foldr" ((t1 ->- t2 ->- t2) ->- t2 ->- tList t1 ->- t2) $ 
         List.foldr
cFoldl1 = mkTerm "foldl1" ((t ->- t ->- t) ->- tList t ->- t) $ foldl1
cFoldr1 = mkTerm "foldr1" ((t ->- t ->- t) ->- tList t ->- t) $ foldr1
cInts =  [ cInt2Expr i | i <- [-10..10]]
cDoubles =  [ cDouble2Expr i | i <- [-10..10]]
cChars = [ cChar2Expr c | c <- ['a'..'z']]

-- | Bools
cNand = mkTerm "nand" (tBool ->- tBool ->- tBool) $ \ x y -> not (x && y)
cAnd  = mkTerm "and"  (tBool ->- tBool ->- tBool) $ \ x y -> (x && y)
cOr   = mkTerm "or"   (tBool ->- tBool ->- tBool) $ \ x y -> (x || y)
cNot  = mkTerm "not"  (tBool ->- tBool) $ \ x -> not (x)

-- | Conditional
cIf = mkTerm "If" (tBool ->- t ->- t ->- t) $ \ p x y -> if p then x else y


-- | Cases
cDefaultCase = mkTerm "defaultCase" (t1 ->- tCase t t1) $ defaultCase 

cAddCase = mkTerm "addCase" (tCase t t1 ->- (t ->- tBool) ->- t1 ->- tCase t t1) $ addCase

cChangeDefault = mkTerm "changeDefault" (tCase t t1 ->- t1 ->- tCase t t1) $ changeDefault

cEvalCase = mkTerm "evalCase" (tCase t t1 ->- t ->- t1) $ evalCase

-- | "Bags", lists which act as collections of objects
cMkSingleton = mkTerm "Singleton" (tInt ->- tList tInt) $ \ x -> [x]

cSetDiff = mkTerm "SetDiff" (tList tInt ->- tList tInt ->- tList tInt) $ ((\\) :: [Int] -> [Int] -> [Int])

cIntersection =  mkTerm "Intersection" (tList tInt ->- tList tInt ->- tList tInt) $ (List.intersect :: [Int] -> [Int] -> [Int])

cIsSingleton = mkTerm "Singleton?" (tList tInt ->- tBool) $ \ x -> (length x) == 1

cUnion =  mkTerm "Union" (tPair (tList tInt) (tList tInt) ->- tList tInt) $ \(x,y) -> x ++ y

bagSelect :: [a] -> ([a],[a])
bagSelect xs = if (null xs) then ([],[]) else ([head xs],(tail xs))
cSelect = mkTerm "Select" (tList tInt ->- tPair (tList tInt) (tList tInt)) $ bagSelect

bagShift :: ([a],[a]) -> ([a],[a])
bagShift (xs,ys) = if (null ys) then (xs,ys) else ((head ys):xs,(tail ys))
cShift = mkTerm "Shift" (tPair (tList tInt) (tList tInt) ->- tPair (tList tInt) (tList tInt)) $ bagShift

-- | Maybe

cJust :: Expr
cJust = mkTerm "Just" (t ->- tMaybe t) $ Just

cNothing :: Expr
cNothing = mkTerm "Nothing" (tMaybe t) $ Nothing

cFromJust :: Expr
cFromJust = mkTerm "fromJust" (tMaybe t ->- t) (\ x -> safeFromJust "cFromJust applied to cNothing" x)

-- | Responses

cNod :: Expr
cNod = mkTerm "Nod" (tBool ->- tResponse) $ Nod

cSpeak :: Expr
cSpeak = mkTerm "Speak" ((tList tChar) ->- tResponse) $ Speak

cGive :: Expr
cGive = mkTerm "Give" ((tList tInt) ->- tResponse) $ Give

cGetNod :: Expr
cGetNod = mkTerm "getNod" (tResponse ->- (tMaybe tBool)) $ getNod

cGetGive :: Expr
cGetGive = mkTerm "getGive" (tResponse ->- (tMaybe (tList tInt))) $ getGive

cGetSpeak :: Expr
cGetSpeak = mkTerm "getSpeak" (tResponse ->- (tMaybe (tList tChar))) $ getSpeak

-- | String Checking
cStrEqual :: Expr
cStrEqual = mkTerm "strEqual" (tList tChar ->- tList tChar ->- tBool) $ ((==) :: String -> String -> Bool)

perceive n = if n == 1 then "a" else "b"
cPerceive = mkTerm "perceive" (tInt ->- tList tChar) $ perceive

objEqual word obj = word == (perceive obj)
cObjEqual = mkTerm "objEqual" (tList tChar ->- tInt ->- tBool) $ objEqual

cIsA = mkTerm "isA" (tInt ->- tBool) $ \x -> (perceive x) == "a"
cIsB = mkTerm "isB" (tInt ->- tBool) $ \x -> (perceive x) == "b"

cQuantCase = mkTerm "quantCase" (tList tChar ->- (tList tInt ->- tList tInt)) $
    evalCase
        (addCase
            (addCase
                (defaultCase (\x -> x))
                (=="ONE")
                (\xs -> fst $ bagSelect xs))
            (=="TWO")
            (\xs -> fst . bagShift $ bagSelect xs))

cNounCase = mkTerm "nounCase" (tList tChar ->- (tList tChar ->- tBool)) $
    evalCase
        (addCase
            (addCase
                (defaultCase (\x -> True))
                (=="A")
                (objEqual "a"))
            (=="B")
            (objEqual "b"))


-- | Hacks for Number Learning

cXHack = mkTerm "x" tInt 1
cOHack = mkTerm "o" tInt 2
cIntEqual = mkTerm "IntEqual" (tInt ->- tInt ->- tBool) $ ((==) :: Int -> Int -> Bool)

-- cIsX = mkTerm "isX" (tInt ->- tBool) $ \x -> x == 1
-- cIsO = mkTerm "isO" (tInt ->- tBool) $ \o -> o == 2

-- | A basic collection of expressions
basicExprs :: [Expr]
basicExprs = [cI, 
              cS, 
              cB, 
              cC, 
              cK, 
--              cBottom, -- Why would we put cBottom in to the library?
              cPlus,
              cTimes, 
              cFPlus,
              cFMinus,
              cFTimes,
              cFDiv,
              cCons, 
              cEmpty,
              cAppend,
              cHead,
              cMap,
              cFoldl,
              cSingle,
              cRep,
              cTail,
              cPair,
              cFst,
              cSnd,
              cBool2Expr True,
              cBool2Expr False,
              cHole
             ] ++ cInts ++ cDoubles ++ cChars


-- | Number Word Learning
numberExprs :: [Expr]
numberExprs = [cFilter, -- Filtering
               cIsSingleton, -- Sets/Worlds
               cSelect,
               cShift,
               cUnion,
               cFst, -- pair
               cSnd,
               cSwap,
               cDefaultCase, -- Cases
               cAddCase,
               cChangeDefault,
               cEvalCase,
               cAnd, -- Booleans
               cOr,
               cNot,
               cBool2Expr True,
               cBool2Expr False,
               stringToExpr "X", -- the word, Strings
               stringToExpr "O",
               stringToExpr "A",
               stringToExpr "B",
               stringToExpr "C",
               stringToExpr "D",
               stringToExpr "E",
               stringToExpr "THING",
               stringToExpr "ONE",
               stringToExpr "TWO",
               stringToExpr "ALL",
               stringToExpr "a", -- the LOT representation
               stringToExpr "b",
               cPerceive,
               cObjEqual,
               cIsA,
               cIsB,
               cStrEqual,
               cQuantCase, -- making it easy
               cNounCase,
               cI, -- combinators
               cS,
               cB,
               cC,
               cK,
               cW] ++ cDualCombinators

               --cCB <> cFilter <> c3Trd
               --cC <> cIntEqual
               --cFilter <> ((cC <> cIntEqual) <> cXHack)

-- Library for testing EM+polynomial regression
polyExprs :: [Expr]
polyExprs = [cI, 
             cS, 
             cB, 
             cC, 
--              cK, 
             cPlus,
             cTimes] ++ [ cInt2Expr 0, cInt2Expr 1 ]

-- Library for tower building
towerExprs :: [Expr]
towerExprs = [cI, 
              cS, 
              cB, 
              cC,
              cK,
              cFPlus,
              cFMinus,
              cFTimes,
              cFDiv,
              cCons,
              cAppend,
              cMap,
              cReverse,
              cSingle,
              cOnFst, cOnSnd,
              cNand,
              cPair, cFst, cSnd--,
--              cHole
              ] ++ [ cDouble2Expr 0, cDouble2Expr 1, cDouble2Expr (-1) ]
                ++ [ cBool2Expr True, cBool2Expr False ]

-- Library for words
wordExprs :: [Expr]
wordExprs = [cI, 
              cS, 
              cB, 
              cC,
              cCons,
              cEmpty,
              cAppend]
              ++ [ cChar2Expr c | c <- ['a'..'z'] ]

mkExprDistr :: [Expr] -> ExprDistr
mkExprDistr exprs = Map.adjust (const (-5)) cBottom
                    $ Map.fromList [(e, 1) | e <- exprs] 

 
-- | A basic expression distribution
basicExprDistr :: ExprDistr
basicExprDistr = Map.adjust (const (-5)) cBottom
                 $ Map.fromList [(e, 1) | e <- basicExprs] 
                 


basicGrammar :: Grammar
basicGrammar = normalizeGrammar $ Grammar (-1.2) basicExprDistr

extendedGrammar :: Grammar
extendedGrammar = normalizeGrammar $ Grammar 3 $ Map.fromList $
                  [(e, 1) | e <- basicExprs] ++ [ (cK <> cI, 1), (cK <> (cInts!!2), 1) ]

-- | Helpers 
-- | compose epressions together
compose :: [Expr] -> Expr
compose = foldl1 (<>)

clearGrammarProbs :: Grammar -> Grammar
clearGrammarProbs Grammar { grExprDistr = distr } =
  Grammar { grExprDistr = Map.map (const $ log 0.5) distr,
            grApp = log 0.5 }

-- | Grammars are in CNF, so each production is the product of two other production
-- Strips out all productions that are subproductions of some other production
-- This is purely for the purpose of making it easier for humans to read grammars
removeSubProductions :: Grammar -> Grammar
removeSubProductions gr@Grammar{grExprDistr = distr} =
  let keys = Map.keys distr
      prods = filter (not . isTerm) keys
      subProductions = map eLeft prods ++ map eRight prods
      prods' = List.nub $ filter (not . flip elem subProductions) prods
      prods'' = prods' ++ filter isTerm keys
  in gr { grExprDistr = Map.filterWithKey (\k v -> k `elem` prods'') distr }

-- | Initializing a TypeInference monad with a Library. We need to
-- grab all type variables in the library and make sure that the type
-- variable counter in the state of the TypeInference monad is greater
-- that that counter.
initializeTI :: Monad m => ExprDistr -> TypeInference m ()
initializeTI exprDistr = modify $ \(_, s) -> (i+1, s)
    where i = maximum $
              concatMap (getTVars . eType . fst) $
              Map.toList exprDistr

-- | Removes productions that aren't used in the P(app)->0 regime
-- Using the old method of calculating production probabilities, these productions wouldn't exist
removeUnusedProductions :: Grammar -> [Expr] -> Grammar
removeUnusedProductions (Grammar { grApp = pApp, grExprDistr = distr }) corpus =
  let used = foldl (\acc -> Set.union acc . grUses) Set.empty corpus
      distr' = Map.filterWithKey (\expr _ -> isTerm expr || expr `Set.member` used) distr
  in Grammar pApp distr'
  where grUses :: Expr -> Set.Set Expr
        grUses expr | Map.member expr distr = Set.singleton expr
        grUses (App { eLeft = l, eRight = r }) =
          Set.union (grUses l) (grUses r)
        grUses expr = error $ "grUses: terminal " ++ show expr ++ " not in library."



countSubtrees :: ExprMap Double -- <cnt>: Map of rules and associated counts.  
                 -> (Expr, Double) -- <expr>: the expression
                 -> ExprMap Double
countSubtrees cnt (expr@(App{eLeft=l, eRight=r}),wt) =
  let cnt'   = incCount cnt (expr,wt)
      cnt''  = countSubtrees cnt' (l,wt)
      cnt''' = countSubtrees cnt'' (r,wt)
  in
   cnt'''
countSubtrees cnt _ = cnt

incCount :: ExprMap Double -> (Expr, Double) 
            -> ExprMap Double
incCount cnt (expr@App{},wt) =
  Map.insertWith (+) expr wt cnt
incCount cnt _ = cnt

-- | Accumulates all of the subtrees in an expression in to a set
collectSubtrees :: Set.Set Expr -> Expr -> Set.Set Expr
collectSubtrees s e@(App { eLeft = l, eRight = r }) =
  collectSubtrees (collectSubtrees (e `Set.insert` s) l) r
collectSubtrees s e = e `Set.insert` s

-- | Simplifies an expression
simplifyExpr :: Expr -> Expr
simplifyExpr expr =
  let loop 0 expr = expr
      loop n e@(Term {}) = e
      loop n e@(App { eLeft = l, eRight = r }) =
        let l' = simplifyExpr l
            r' = simplifyExpr r
            e' = l' <> r'
            e'' = foldl (Prelude.flip ($)) e' patterns
        in
         if e' /= e''
         then loop (n-1) e''
         else e''
  in loop 100 expr
  where
    patterns = map (\(str, proc) -> matchExpr (readExpr str) proc)
                   [ -- Combinator identities
                     ( "((K ?) ?)", \ [x, _] -> x),
                     ( "(((B ?) ?) ?)", \ [f, g, x] -> f <> (g <> x)),
                     ( "(((C ?) ?) ?)", \ [f, g, x] -> (f <> x) <> g),
                     ( "(((S ?) ?) ?d)", \ [f, g, x] -> (f <> x) <> (g <> x)),
                     ( "(I ?)", \ [x] -> x),
                     -- Arithmetic identities
                     ( "((+ 0) ?)", \ [x] -> x),
                     ( "((+ ?) 0)", \ [x] -> x),
                     ( "((* 1) ?)", \ [x] -> x),
                     ( "((* ?) 1)", \ [x] -> x),
                     ( "((* ?) 1)", \ [x] -> x),
                     ( "((* 0) ?)", \ [_] -> cInt2Expr 0),
                     ( "((* ?) 0)", \ [_] -> cInt2Expr 0),
                     -- Evaluation
                     ( "((+ ?t) ?t)", \ [x, y] -> cInt2Expr ((eval x) + (eval y))),
                     ( "((* ?t) ?t)", \ [x, y] -> cInt2Expr ((eval x) * (eval y))) ]


-- | Saves a grammar to a file
saveGrammar :: String -> Grammar -> IO ()
saveGrammar fname (Grammar papp distr) =
  writeFile fname $ show papp ++ "\n" ++ prods
  where prods = unlines $ map (\(c, p) -> show p ++ " " ++ show c) $ Map.toList distr

-- | Loads a grammar from a file
loadGrammar :: String -> IO Grammar
loadGrammar fname = do
  fcontents <- readFile fname
  let (papp : prods) = lines fcontents
  let prods' = map (\ln ->
                     let p = read $ takeWhile (/=' ') ln
                         c = readExpr $ drop 1 $ dropWhile (/=' ') ln
                         c' = c { eType = doTypeInference_ c }
                     in (c', p)) prods
  return $ Grammar { grApp = read papp,
                     grExprDistr = Map.fromList prods' }
  

-- | Looks for the largest file of the form grammar_#, and returns the grammar and the number
loadNextGrammar :: IO (Grammar, Int)
loadNextGrammar = do
  contents <- getDirectoryContents "."
  let contents' = filter (\c -> take 8 c == "grammar_") contents
  let latest = List.maximumBy (\c c' -> compare (read (drop 8 c)) ((read (drop 8 c)) :: Int)) contents'
  let num = read $ drop 8 latest
  gr <- loadGrammar latest
  return (gr, num)

  
  

-- | Simplifies terms in the grammar, subject to the constraint that it reduces description length
-- Does a greedy, best-first search
simplifyLibrary :: [Expr] -> -- ^ library productions
                   ([Expr], -- ^ new library productions
                    [(Expr, Expr)]) -- ^ substitutions made
simplifyLibrary prods =
  let (newprods, subs) = simplify prods $ score prods
      newprods' = Set.toList $ foldl (\acc prod -> collectSubtrees acc prod) Set.empty newprods
  in (map (\prod -> prod { eType = doTypeInference_ prod}) newprods', subs)
  where -- Score counts # unique subtrees
        score = Set.size . foldl (\acc prod -> collectSubtrees acc prod) Set.empty
        -- Takes as input a library and its score
        simplify lib libScore =
          let simplifiedLib = filter (\ (x, y) -> x /= y) $ map (\prod -> (prod, simplifyExpr prod)) lib
              newLibs = map (\(prod, simpProd) -> let lib' = List.nub $ map (subExpr prod simpProd) lib
                                                  in ((lib', (prod, simpProd)), score lib')) simplifiedLib
              ((newLib, newSub), newScore) = List.minimumBy (compare `on` snd) newLibs
          in
           if newScore < libScore
           then trace ("Improved score from " ++ show libScore ++ " to " ++ show newScore)
                      (let (bestLib, bestSubs) = simplify newLib newScore in (bestLib, newSub:bestSubs))
           else (lib, [])

readExpr :: String -> Expr
readExpr input = case parse parseComb "CL" input of
     Left err -> error $ "No match: " ++ show err
     Right val -> val
     where symbol :: Parser Char
           symbol = oneOf "!#$%&|*+-/:<=>?@^_~.[]?'"
           parseAtom :: Parser Expr
           parseAtom = do 
             hd <- letter <|> digit <|> symbol
             tl <- many (letter <|> digit <|> symbol)
             let atom = hd:tl
             case hd of
               '?' -> return $ Term atom undefined undefined -- wildcard for pattern matching
               _ ->
                 return $ case List.find (\c -> show c == atom) basicExprs of
                   Nothing -> error $ "Could not find in library: " ++ show atom
                   Just e -> e
           parseApp :: Parser Expr
           parseApp = do
             char '('
             f <- parseComb
             char ' '
             a <- parseComb
             char ')'
             return $ f <> a
           parseComb :: Parser Expr
           parseComb = parseAtom <|> parseApp
