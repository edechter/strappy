-- Routers.hs

module Strappy.Routers where

import Strappy.Type
import Strappy.CL
import Strappy.Expr
import qualified Strappy.CombMap  as CM


-- | define some oft used type variables
t = mkTVar 0
t0 = mkTVar 0 
t1 = mkTVar 1
t2 = mkTVar 2
t3 = mkTVar 3
t4 = mkTVar 4
t5 = mkTVar 5
t6 = mkTVar 6
t7 = mkTVar 7
t8 = mkTVar 8

-- router lib
routers = one_routers `CM.union` two_routers `CM.union` three_routers
                  

one_routers = CM.fromList 
          $ [(cName c, c) | c <- [cS, cC, cB]]
two_routers = CM.fromList 
              $ [(cName c, c) | c <- [cSS, cSB, cSC, 
                  cBS, cBB, cBC, cCS, cCB, cCC]]
three_routers = CM.fromList 
          $ [(cName c, c) | c <- ternaryRouters]


-- convenience definations
infixl 4 <+>
(<+>) = App 

cS = CNode "S" (Func $ \f ->
                Func $ \g -> 
                Func $ \x ->
                (f <+> x ) <+> (g <+> x)) tp
     where tp = (t2 ->- t1 ->- t0) ->- (t2 ->- t1) ->- t2 ->- t0

cB = CNode "B" (Func $ \f ->
                Func $ \g -> 
                Func $ \x ->
                f <+> (g <+> x)) tp
     where tp = (t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t

cC = CNode "C" (Func $ \f ->
                Func $ \g -> 
                Func $ \x ->
                (f <+> x ) <+> g) tp
     where tp = (t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t 

cSS = CNode "SS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x1 <+> x2 ) <+> (g <+> x1 <+> x2)) tp
     where tp = (t2 ->- t3 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t

cSB = CNode "SB" (Func $ \f ->
                  Func $ \g -> 
                  Func $ \x1 ->
                  Func $ \x2 ->
                  (f <+> x1 ) <+> (g <+> x1 <+> x2)) tp
     where tp = (t2 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t
cSC = CNode "SC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x1 <+> x2 ) <+> (g <+> x1)) tp
     where tp = (t3 ->- t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t3 ->- t1 ->- t

cBS = CNode "BS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x2 ) <+> (g <+> x1 <+> x2)) tp
     where tp = (t3 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t

cBB = CNode "BB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                f <+> (g <+> x1 <+> x2)) tp
     where tp = (t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t

cBC = CNode "BC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x2) <+> (g <+> x1)) tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t3 ->- t1 ->- t

cCS = CNode "CS" (Func $ \f ->
                  Func $ \g -> 
                  Func $ \x1 ->
                  Func $ \x2 ->
                  (f <+> x1 <+> x2) <+> (g <+> x2)) tp
     where tp = (t1 ->- t3 ->- t2 ->- t) ->- (t3 ->- t2) ->- t1 ->- t3 ->- t

cCB = CNode "CB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x1) <+> (g <+> x2)) tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t1 ->- t3 ->- t

cCC = CNode "CC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x1 <+> x2) <+> (g)) tp
     where tp = (t1 ->- t2 ->- t3 ->- t) ->- t3 ->- t1 ->- t2 ->- t

cSSS = CNode "SSS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g <+> x1 <+> x2 <+> x3)) tp
     where tp = (t2 ->- t3 ->- t4 ->- t1 ->- t)
              ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cSSB = CNode "SSB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g <+> x1 <+> x2 )) tp
     where tp = (t3 ->- t4 ->- t1 ->- t2 ->- t)
              ->- (t3 ->- t4 ->- t2) ->- t3 ->- t4 ->- t1 ->- t

cSSC = CNode "SSC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 ) <+> (g <+> x1 <+> x2 <+> x3 )) tp
     where tp = (t2 ->- t3 ->- t1 ->- t)
              ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t


cSBS = CNode "SBS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g <+> x1 <+> x3 )) tp
    where tp = (t3 ->- t1 ->- t4 ->- t2 ->- t)
             ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t

cSBB = CNode "SBB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g <+> x1 )) tp
     where tp =  (t4 ->- t1 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t4 ->- t1 ->- t2 ->- t

cSBC = CNode "SBC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2) <+> (g <+> x1 <+> x3 )) tp
     where tp = (t3 ->- t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t

cSCS = CNode "SCS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x3) <+> (g <+> x1 <+> x2 <+> x3 )) tp
     where tp = (t2 ->- t4 ->- t1 ->- t)
              ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cSCB = CNode "SCB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x3) <+> (g <+> x1 <+> x2  )) tp
     where tp = (t3 ->- t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t4 ->- t1 ->- t


cSCC = CNode "SCC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1) <+> (g <+> x1 <+> x2 <+> x3  )) tp
     where tp = (t2 ->- t1 ->- t) ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cBSS = CNode "BSS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g  <+> x2 <+> x3  )) tp
     where tp = (t1 ->- t3 ->- t4 ->- t2 ->- t)
              ->- (t3 ->- t4 ->- t2) ->- t1 ->- t3 ->- t4 ->- t

cBSB = CNode "BSB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g  <+> x2   )) tp
     where tp = (t1 ->- t4 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t1 ->- t4 ->- t2 ->- t

cBSC = CNode "BSC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 ) <+> (g  <+> x2 <+> x3   )) tp
     where tp = (t1 ->- t3 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t1 ->- t3 ->- t4 ->- t

cBBS = CNode "BBS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3 ) <+> (g  <+> x3   )) tp
     where tp = (t1 ->- t2 ->- t4 ->- t3 ->- t) ->- (t4 ->- t3) ->- t1 ->- t2 ->- t4 ->- t

cBBB = CNode "BBB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3 ) <+> g ) tp
     where tp = (t1 ->- t2 ->- t3 ->- t4 ->- t) ->- t4 ->- t1 ->- t2 ->- t3 ->- t


cBBC = CNode "BBC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2  ) <+> (g <+> x3) ) tp
     where tp = (t1 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t1 ->- t2 ->- t4 ->- t

cBCS = CNode "BCS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x3  ) <+> (g <+> x2 <+> x3) ) tp
     where tp = (t1 ->- t4 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t1 ->- t3 ->- t4 ->- t

cBCB = CNode "BCB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x3  ) <+> (g <+> x2) ) tp
     where tp = (t1 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t1 ->- t4 ->- t2 ->- t

cBCC = CNode "BCC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1   ) <+> (g <+> x2 <+> x3) ) tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t1 ->- t3 ->- t4 ->- t

cCSS = CNode "CSS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 <+> x3   ) <+> (g <+> x1 <+> x2 <+> x3) ) tp
     where tp = (t3 ->- t4 ->- t1 ->- t)
              ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cCSB = CNode "CSB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 <+> x3   ) <+> (g <+> x1 <+> x2) ) tp
     where tp = (t4 ->- t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t4 ->- t1 ->- t

cCSC = CNode "CSC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2    ) <+> (g <+> x1 <+> x2 <+> x3) ) tp
     where tp = (t3 ->- t1 ->- t) ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cCBS = CNode "CBS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 <+> x3) <+> (g <+> x1  <+> x3) ) tp
     where tp = (t1 ->- t4 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t

cCBB = CNode "CBB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 <+> x3) <+> (g <+> x1) ) tp
     where tp = (t1 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t4 ->- t1 ->- t2 ->- t

cCBC = CNode "CBC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 ) <+> (g <+> x1 <+> x3) ) tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t

cCCS = CNode "CCS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x3 ) <+> (g <+> x1 <+> x2 <+> x3) ) tp
     where tp = (t4 ->- t1 ->- t) ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cCCB = CNode "CCB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x3 ) <+> (g <+> x1 <+> x2 ) ) tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t4 ->- t1 ->- t

cCCC = CNode "CCC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f ) <+> (g <+> x1 <+> x2 <+> x3) ) tp
     where tp = (t1 ->- t) ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

ternaryRouters = [cSSS
                 , cSSB
                 , cSSC
                 , cSBS
                 , cSBB
                 , cSBC
                 , cSCS
                 , cSCB
                 , cSCC
                 , cBSS
                 , cBSB
                 , cBSC
                 , cBBS
                 , cBBB
                 , cBBC
                 , cBCS
                 , cBCB
                 , cBCC
                 , cCSS
                 , cCSB
                 , cCSC
                 , cCBS
                 , cCBB
                 , cCBC
                 , cCCS
                 , cCCB
                 , cCCC]