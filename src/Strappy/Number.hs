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

aHack = 1
bHack = 2

logInt = log . fromIntegral

world2Expr = intListToExpr

makeBag :: Int -> Int -> [Int]
makeBag a b = (replicate a aHack) ++ (replicate b bHack)

allUniqueWorlds as bs = 
 (nub $ concat [permutations world | world <- [makeBag a b |
                                     a <- as,
                                     b <- bs]])

checkTask :: (String,String,[Int],Maybe [Int]) -> Bool
checkTask (_,_,_,Nothing) = False
checkTask ("ALL",noun,world,(Just result)) =
    case noun of
        "A"     -> result == (filter (==aHack) world)
        "B"     -> result == (filter (==bHack) world)
        "thing" -> (filter (== aHack) result) == (filter (==aHack) world) &&
                   (filter (== bHack) result) == (filter (==bHack) world) &&
                   (length result) == (length world)
        _       -> False
checkTask ("ONE",noun,world,(Just result)) =
    case noun of
        "A"     -> result == [aHack]
        "B"     -> result == [bHack]
        "thing" -> result == [aHack] || result == [bHack]
        _       -> False
checkTask ("TWO",noun,world,(Just result)) =
    case noun of
        "A"     -> result == [aHack,aHack]
        "B"     -> result == [bHack,bHack]
        "THING" -> result == [aHack,aHack] || result == [bHack,bHack] ||
                   result == [aHack,bHack] || result == [bHack,aHack]
        _       -> False

tallyResults :: [(String,String,[Int],Maybe [Int])] -> Int
tallyResults results = 
    let corrects = filter checkTask results
        nCorrect = length corrects
        nQuants  = length . nub $ map (\(q,n,w,r) -> q) corrects
        nNouns   = length . nub $ map (\(q,n,w,r) -> n) corrects
        nWorlds  = length . nub $ map (\(q,n,w,r) -> 
          (length $ filter (==aHack) w,length $ filter (==bHack) w)) corrects
    in nCorrect * nQuants * nNouns * nWorlds

tallyNames :: [(Int,Maybe String)] -> Int
tallyNames results =
    let checkNames (n,Just s) =
           s == case n of
                1 -> "A"
                2 -> "B"
                3 -> "C"
                4 -> "D"
                5 -> "E"
                _ -> "THING"
        checkNames (n,_) = False
    in length $ filter checkNames results

llExprUnderGrammar :: Grammar -> Expr
llExprUnderGrammar grammar =
    let goalType = ((tList tChar) ->- (tList tChar) ->- (tList tInt) ->- (tList tInt))
        goalProgram = (cC <>
                          (cB <>
                              ((cB <> cB) <> cB) <>
                              cQuantCase) <>
                          (cB <>
                              cFilter <>
                              cNounCase))
    in exprLogLikelihood grammar $ annotateRequested' goalType goalProgram

saveLLImprovement :: String -> Int -> Expr -> IO ()
saveLLImprovement prefix iter expr =
   let io = if iter < 0 then writeFile else appendFile
       eLL = safeFromJust "Expression has no likelihood!" $ eLogLikelihood expr
   in io (prefix ++ "goalLL") ((show iter) ++ ", " ++ (show eLL))

chooseName qs ns as bs =
    let nameStr ls = if ((length ls) == 1) then (head ls) else "*" 
        nameInt ls = if ((length ls) == 1) then (show $ head ls) else "*" 
    in intercalate "_" ["task", nameStr qs, nameStr ns, nameInt as, nameInt bs]

-- | TASK | --

-- makeTask :: [String] -> [String] -> [Int] -> [Int] -> EMTask
-- makeTask quants nouns as bs =
--     EMTask { etName = chooseName quants nouns as bs,
--              etLogLikelihood = \e ->
--                 let results = [(quant,noun,world,timeLimitedEval
--                                   (e <> 
--                                       (stringToExpr quant) <>
--                                       (stringToExpr  noun) <>
--                                       (world2Expr world))) |
--                                           world <- (allUniqueWorlds as bs),
--                                           noun <- nouns,
--                                           quant <- quants] :: [(String,String,[Int],Maybe [Int])]
--                 -- hack! I use a constant to cover all possible worlds here, weighted to favor generalization
--                 in (logInt $ tallyResults results) - (logInt $ 81 * 558),
--              etType = (tList tChar) ->- (tList tChar) ->- (tList tInt) ->- (tList tInt) }

makeNameTask :: EMTask
makeNameTask =
    EMTask { etName = "task_name",
             etLogLikelihood = \e ->
                 let results = [(n,timeLimitedEval (e <> intToExpr n)) | n <- [1..6]] :: [(Int,Maybe String)]
                 in (logInt $ tallyNames results) - (logInt $ length results),
             etType = (tInt ->- tList tChar)}

-- | MAIN | --

-- lister ls = listMaker [] $ reverse ls
--     where listMaker rs [] = rs
--           listMaker rs ls = listMaker [r:l | r <- rs, l <- (head ls)] (tail ls)

main = do
--  args@[nTypes, nObjects, rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
--  let nt = read nTypes
--  let no = read nObjects
    let seed = Grammar { grApp = log 0.375,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberExprs ] }
--      objs = replicate nt (take no [1..])
--      qs = take n ["ONE","TWO","ALL","THREE","NONE"]
--      ns = take n ["A","B","THING","C","D"]
        tasks = [makeNameTask]
                -- [makeTask qs ns objs] ++ -- (*,*,*)
                -- [makeTask [q] ns objs   | q <- qs] ++ -- (q,*,*)
                -- [makeTask qs [n] objs   | n <- ns] ++ -- (*,n,*)
                -- [makeTask qs ns [obj]  | a <- as, b <- bs] ++ -- (*,*,w)
                -- [makeTask qs [n] [a] [b] | n <- ns, a <- as, b <- bs] ++ -- (*,n,w)
                -- [makeTask [q] ns [a] [b] | q <- qs, a <- as, b <- bs] ++ -- (q,*,w)
                -- [makeTask [q] [n] as bs  | q <- qs, n <- ns] ++ -- (q,n,*)
                -- [makeTask [q] [n] [a] [b]| q <- qs, n <- ns, a <- as, b <- bs] ++ -- (q,n,w)
               
    saveLLImprovement (prefix ++ "/") (-1)$ llExprUnderGrammar seed
    good <- loopM seed [0..19] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix ++ "/" ++ show step) tasks 
            (read lambda) (read pseudocounts) (read fSize) seed grammar  
        let grammar'' = normalizeGrammar grammar'
        saveGrammar (prefix ++ "/" ++ show step ++ "_grammar") grammar''
        saveLLImprovement (prefix ++ "/") step $ llExprUnderGrammar grammar''
        return grammar''
    putStrLn $ "Finished!"
