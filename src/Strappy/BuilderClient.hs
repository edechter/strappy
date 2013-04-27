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


-- | Given a plan and a list of outcomes, evaluates to a list of final heights
-- The outcomes may be taken from requestConstruction, for example
computeFinalHeights :: [(Double,Double,Double,Double)] ->
                       [[(Double,Double,Double)]] ->
                       [Double]
computeFinalHeights plan outcomes =
  let blockRadiiAndTheta = map (\(_,_,dx,dz) ->
                                (sqrt $ dx*dx + dz*dz, dTheta = atan2 dz dx))
                               plan
  in flip map outcomes $
     \outcome -> maximum $ zipWith (\(r,dTheta) (_,z,ang) ->
                                     blockHeight r dTheta z ang)
                                   blockRadiiAndTheta outcome
  where blockHeight r dTheta z ang =
          let ang' = ang / 180.0 * pi
              r = 
              
              -- Check all four corners
          in maximum [ z + r * sin (ang + dTheta),
                       z + r * sin (ang - dTheta),
                       z + r * sin (pi + ang + dTheta),
                       z + r * sin (pi + ang - dTheta) ]
