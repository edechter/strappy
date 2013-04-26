import Network
import System.IO

-- | Sends a construction to a builder server, along with the amount to shove
-- A construction plan is a list of tuples of (x, z, dx, dz)
--     where x, z are coordinates and dx, dz half extents
-- The amount(s) by which to shove the table are given as a list of impulses
-- For each impulse, the final locations, given as (x, z, angle) of each block,
--     are returned from the server
requestConstruction :: String ->
                       [(Double, Double, Double, Double)] ->
                       [Double] ->
                       IO [[(Double, Double, Double)]]
requestConstruction host plan shoves = withSocketsDo $ do
  -- Connect on port 1540
  h <- connectTo host (PortNumber 1540)
  hSetBuffering h LineBuffering
  -- Delimit plan from the perturbations via a pipe
  hPutStr h $ show plan ++ "|" ++ show shoves ++ "\n"
  result <- hGetContents h
  print result
  hClose h
  return $ read result
