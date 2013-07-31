-- Kevin Ellis, 2013. 6.868 Final Project

module Physics.BlockClient where


import System.IO
import System.Process

gnd_height :: Double
gnd_height = -3.0

-- | Wrapper function:
-- Takes a plan, as output by a combinator,
-- and returns the height and fractions of times stable
runPlan :: [(Double, Bool)] -> [Double] ->
           IO (Double, [Double])
runPlan plan str | any (\(x, _) -> isNaN x || isInfinite x) plan = return (log 0, replicate (length str) 0.0)
runPlan plan strength = do
  let dx = 1.5
  let dz = 0.25
  let plan' = map (\(x, stablep) -> if stablep then (x, dx, dz) else (x, dz, dx)) plan
  requestLocalConstruction plan' strength

-- | Sends a construction to a builder script, along with the amount to shove
-- A construction plan is a list of tuples of (x, dx, dz)
--     where x, is a coordinate and dx, dz half extents
-- The amount(s) by which to shove the table are given as a list of impulses
-- For each impulse, the final locations, given as (x, z, angle) of each block,
--     are returned from the server
requestLocalConstruction :: [(Double, Double, Double)] ->
                            [Double] ->
                            IO (Double, [Double])
requestLocalConstruction [] str = return (log 0, replicate (length str) 0.0)
requestLocalConstruction plan shoves = do
  putStrLn $ "About to run simulator...\t" ++ show plan
  response <- readProcess "./simulator/console.py" [show plan ++ "|" ++ show shoves] ""
  putStrLn $ show plan ++ "  -->  " ++ response
  if head response /= '[' -- failure to build tower
    then return (log 0, replicate (length shoves) 0.0)
    else do let (ht:percentStable) = read response
            let percentStable' = percentStable ++ (replicate (length shoves - length percentStable) 0.0)
            return (ht, map (/100.0) percentStable')

