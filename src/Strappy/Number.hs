module Main where

import Strappy.Config
import Strappy.Expr
import Strappy.Type
import Strappy.Utils
import Strappy.Library
import Strappy.BeamSearch
import Strappy.Planner
import Strappy.EM
import Strappy.ProdFeat
import Strappy.Sample
import Strappy.EnumBits
import Strappy.Response

import qualified Data.Map as Map
import System.Environment
import System.Random
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.List
import Data.Array

-- | TASK SPECIFIC UTILITIES | --

xHack = 1
oHack = 2

logInt = log . fromIntegral

world2Expr = intListToExpr

makeBag :: Int -> Int -> [Int]
makeBag x o = (replicate x xHack) ++ (replicate o oHack)

allUniqueWorlds xs os = 
 (nub $ concat [permutations world | world <- [makeBag x o |
                                     x <- xs,
                                     o <- os]])

checkTask :: (String,String,[Int],Maybe [Int]) -> Bool
checkTask (_,_,_,Nothing) = False
checkTask ("all",noun,world,(Just result)) =
    case noun of
        "X"     -> result == (filter (==xHack) world)
        "O"     -> result == (filter (==oHack) world)
        "thing" -> (filter (== xHack) result) == (filter (==xHack) world) &&
                   (filter (== oHack) result) == (filter (==oHack) world) &&
                   (length result) == (length world)
        _       -> False
checkTask ("one",noun,world,(Just result)) =
    case noun of
        "X"     -> result == [xHack]
        "O"     -> result == [oHack]
        "thing" -> result == [xHack] || result == [oHack]
        _       -> False
checkTask ("two",noun,world,(Just result)) =
    case noun of
        "X"     -> result == [xHack,xHack]
        "O"     -> result == [oHack,oHack]
        "thing" -> result == [xHack,xHack] || result == [oHack,oHack] ||
                   result == [xHack,oHack] || result == [oHack,xHack]
        _       -> False

tallyResults :: [(String,String,[Int],Maybe [Int])] -> Int
tallyResults results = 
    let corrects = filter checkTask results
        nCorrect = length corrects
        nQuants  = length . nub $ map (\(q,n,w,r) -> q) corrects
        nNouns   = length . nub $ map (\(q,n,w,r) -> n) corrects
        nWorlds  = length . nub $ map (\(q,n,w,r) -> 
          (length $ filter (==xHack) w,length $ filter (==oHack) w)) corrects
    in nCorrect * nQuants * nNouns * nWorlds

llExprUnderGrammar :: Grammar -> Expr
llExprUnderGrammar grammar =
    let goalType = (tTriple (tList tChar) (tList tChar) (tList tInt) ->- (tList tInt))
        goalProgram = (cS <> 
                        (cB <> 
                            (cEvalCase <> 
                                (cAddCase <> 
                                    (cAddCase <>
                                        (cDefaultCase <>
                                            cI) <> 
                                        (cStrEqual <> stringToExpr "one") <> 
                                        (cB <> cFst <> cSelect)) <> 
                                    (cStrEqual <> stringToExpr "two") <> 
                                    (cB <> cFst <> (cB <> cShift <> cSelect)))) <> 
                            c3Fst) <> 
                        (cS <> 
                            (cB <> 
                                cFilter <> 
                                    (cB <> 
                                        (cEvalCase <> 
                                            (cAddCase <> 
                                                (cAddCase <>
                                                    (cDefaultCase <>
                                                        (cK <> cBool2Expr True)) <> 
                                                    (cStrEqual <> stringToExpr "X") <> 
                                                    (cObjEqual <> stringToExpr "x")) <> 
                                                (cStrEqual <> stringToExpr "O") <> 
                                                (cObjEqual <> stringToExpr "o"))) <> 
                                        c3Snd)) <> 
                            c3Trd))
    in exprLogLikelihood grammar $ annotateRequested' goalType goalProgram

saveLLImprovement :: String -> Expr -> IO ()
saveLLImprovement prefix expr =
    writeFile (prefix++"_llImprovement") (show $ eLogLikelihood expr)

chooseName qs ns xs os =
    let nameStr ls = if ((length ls) == 1) then (head ls) else "*" 
        nameInt ls = if ((length ls) == 1) then (show $ head ls) else "*" 
    in intercalate "_" ["task", nameStr qs, nameStr ns, nameInt xs, nameInt os]

-- | TASK | --

makeTask :: [String] -> [String] -> [Int] -> [Int] -> EMTask
makeTask quants nouns xs os =
    EMTask { etName = chooseName quants nouns xs os,
             etLogLikelihood = \e ->
                let results = [(quant,noun,world,timeLimitedEval
                                  (e <> 
                                      (cTriple             <>
                                      (stringToExpr quant) <>
                                      (stringToExpr  noun) <>
                                      (world2Expr world)))) |
                                          world <- (allUniqueWorlds xs os),
                                          noun <- nouns,
                                          quant <- quants] :: [(String,String,[Int],Maybe [Int])]
                -- hack! I use a constant to cover all possible worlds here, weighted to favor generalization
                in (logInt $ tallyResults results) - (logInt $ 81 * 558),
             etType = tTriple (tList tChar) (tList tChar) (tList tInt) ->- (tList tInt) }

-- | MAIN | --

main = do
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.375,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberExprs ] }
        xs = [1..3] -- change hacked value
        os = [1..3] -- change hacked value
        qs = ["one","two","all"] -- change hacked value
        ns = ["X","O","thing"] -- change hacked value
        tasks = [makeTask qs ns xs os] ++ -- (*,*,*)
                [makeTask [q] ns xs os   | q <- qs] ++ -- (q,*,*)
                [makeTask qs [n] xs os   | n <- ns] ++ -- (*,n,*)
                [makeTask qs ns [x] [o]  | x <- xs, o <- os] ++ -- (*,*,w)
                [makeTask qs [n] [x] [o] | n <- ns, x <- xs, o <- os] ++ -- (*,n,w)
                [makeTask [q] ns [x] [o] | q <- qs, x <- xs, o <- os] ++ -- (q,*,w)
                [makeTask [q] [n] xs os  | q <- qs, n <- ns] ++ -- (q,n,*)
                [makeTask [q] [n] [x] [o]| q <- qs, n <- ns, x <- xs, o <- os] -- (q,n,w)
    good <- loopM seed [0..19] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix ++ "/" ++ show step) "task_one_O_3_2" tasks 
            (read lambda) (read pseudocounts) (read fSize) seed grammar  
        let grammar'' = normalizeGrammar grammar'
        saveGrammar (prefix ++ "/" ++ show step ++ "_grammar") grammar''
        saveLLImprovement (prefix ++ "/" ++ show step) $ llExprUnderGrammar grammar''
        return grammar''
    putStrLn $ "Finished!"
