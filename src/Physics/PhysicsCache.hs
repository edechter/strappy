-- Kevin Ellis, 2013. 6.868 Final Project

module Physics.PhysicsCache where

import Physics.BlockClient

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as M

type Plan = [(Double, Bool)]
type WorldState = [(Double, Double, Double)]

data PhysicsCache = PhysicsCache { physStable :: M.Map (Plan, Double, Double) Bool,
                                   physWState :: M.Map Plan (WorldState, Double) }
                  deriving(Read, Show)

type SharedCache = MVar PhysicsCache

newPhysicsCache = newMVar $ PhysicsCache { physStable = M.empty, physWState = M.empty }

canonicalizePlan :: Plan -> Plan
canonicalizePlan xs =
  let leftmost = minimum $ map fst xs in
  map (\(x,o) -> (x-leftmost, o)) xs

cachedPlan :: SharedCache -> [(Double,Bool)] -> IO ([(Double, Double, Double)], Double)
cachedPlan cache plan = do
  cache' <- readMVar cache
  case M.lookup plan (physWState cache') of
    Just x -> return x
    Nothing -> do outcome <- runPlan plan 0.0
                  modifyMVar_ cache ( \c -> return $ c { physWState = M.insert plan outcome (physWState c) })
                  return outcome

cachedPerturb :: SharedCache -> Double -> Double -> [(Double,Bool)] -> IO Bool
cachedPerturb cache strength goalHeight plan = do
  cache' <- readMVar cache
  let plan' = canonicalizePlan plan
  case M.lookup (plan', strength, goalHeight) (physStable cache') of
    Just x -> return x
    Nothing -> do (heights,_) <- runPlan plan strength
                  let score = length $ filter (>goalHeight) $ map (\(h,_,_) -> h) heights
                  let didWeWin = score >= 240  -- have to get at least 80% right
                  modifyMVar_ cache $
                    \c -> return $ c { physStable = M.insert (plan', strength, goalHeight) didWeWin (physStable c) }
                  return didWeWin

savePhysicsCache :: SharedCache -> String -> IO ()
savePhysicsCache cache fname = do
  cache' <- readMVar cache
  writeFile fname $ show cache'

loadPhysicsCache :: String -> IO SharedCache
loadPhysicsCache fname = do
  contents <- readFile fname
  newMVar $ read contents