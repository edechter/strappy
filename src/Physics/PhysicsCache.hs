-- Kevin Ellis, 2013. 6.868 Final Project

module Physics.PhysicsCache where

import Physics.BlockClient

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as M

type Plan = [(Double, Bool)]
type WorldState = [(Double, Double, Double)]

data PhysicsCache = PhysicsCache { physStable :: M.Map Plan (Double, [Double]) }
                  deriving(Read, Show)

type SharedCache = MVar PhysicsCache

newPhysicsCache = newMVar $ PhysicsCache { physStable = M.empty }

canonicalizePlan :: Plan -> Plan
canonicalizePlan xs =
  let leftmost = minimum $ map fst xs in
  map (\(x,o) -> (x-leftmost, o)) xs

cachedPerturb :: SharedCache -> -- ^ Physics cache
                 [Double] -> -- ^ Perturbation strengths
                 [(Double,Bool)] -> -- ^ Plan
                 IO (Double, [Double]) -- ^ Height, fraction-stable-for-each-perturbation
cachedPerturb cache perturbs plan = do
  cache' <- readMVar cache
  let plan' = canonicalizePlan plan
  case M.lookup plan' (physStable cache') of
    Just x -> return x
    Nothing -> do result <- runPlan plan perturbs
                  modifyMVar_ cache $
                    \c -> return $ c { physStable = M.insert plan' result (physStable c) }
                  return result

savePhysicsCache :: SharedCache -> String -> IO ()
savePhysicsCache cache fname = do
  cache' <- readMVar cache
  writeFile fname $ show cache'

loadPhysicsCache :: String -> IO SharedCache
loadPhysicsCache fname = do
  contents <- readFile fname
  newMVar $ read contents