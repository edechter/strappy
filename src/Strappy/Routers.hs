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

cS = CLeaf "S" (Func $ \f ->
                Func $ \g -> 
                Func $ \x ->
                (f <+> x ) <+> (g <+> x)) tp tp
     where tp = (t2 ->- t1 ->- t0) ->- (t2 ->- t1) ->- t2 ->- t0

cB = CLeaf "B" (Func $ \f ->
                Func $ \g -> 
                Func $ \x ->
                f <+> (g <+> x)) tp tp
     where tp = (t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t

cC = CLeaf "C" (Func $ \f ->
                Func $ \g -> 
                Func $ \x ->
                (f <+> x ) <+> g) tp tp
     where tp = (t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t 

cSS = CLeaf "SS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x1 <+> x2 ) <+> (g <+> x1 <+> x2)) tp tp
     where tp = (t2 ->- t3 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t

cSB = CLeaf "SB" (Func $ \f ->
                  Func $ \g -> 
                  Func $ \x1 ->
                  Func $ \x2 ->
                  (f <+> x1 ) <+> (g <+> x1 <+> x2)) tp tp
     where tp = (t2 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t
cSC = CLeaf "SC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x1 <+> x2 ) <+> (g <+> x1)) tp tp
     where tp = (t3 ->- t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t3 ->- t1 ->- t

cBS = CLeaf "BS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x2 ) <+> (g <+> x1 <+> x2)) tp tp
     where tp = (t3 ->- t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t

cBB = CLeaf "BB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                f <+> (g <+> x1 <+> x2)) tp tp
     where tp = (t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t

cBC = CLeaf "BC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x2) <+> (g <+> x1)) tp tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t3 ->- t1 ->- t

cCS = CLeaf "CS" (Func $ \f ->
                  Func $ \g -> 
                  Func $ \x1 ->
                  Func $ \x2 ->
                  (f <+> x1 <+> x2) <+> (g <+> x2)) tp tp
     where tp = (t1 ->- t3 ->- t2 ->- t) ->- (t3 ->- t2) ->- t1 ->- t3 ->- t

cCB = CLeaf "CB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x1) <+> (g <+> x2)) tp tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t1 ->- t3 ->- t

cCC = CLeaf "CC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                (f <+> x1 <+> x2) <+> (g)) tp tp
     where tp = (t1 ->- t2 ->- t3 ->- t) ->- t3 ->- t1 ->- t2 ->- t

cSSS = CLeaf "SSS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g <+> x1 <+> x2 <+> x3)) tp tp
     where tp = (t2 ->- t3 ->- t4 ->- t1 ->- t)
              ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cSSB = CLeaf "SSB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g <+> x1 <+> x2 )) tp tp
     where tp = (t3 ->- t4 ->- t1 ->- t2 ->- t)
              ->- (t3 ->- t4 ->- t2) ->- t3 ->- t4 ->- t1 ->- t

cSSC = CLeaf "SSC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 ) <+> (g <+> x1 <+> x2 <+> x3 )) tp tp
     where tp = (t2 ->- t3 ->- t1 ->- t)
              ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t


cSBS = CLeaf "SBS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g <+> x1 <+> x3 )) tp tp
    where tp = (t3 ->- t1 ->- t4 ->- t2 ->- t)
             ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t

cSBB = CLeaf "SBB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g <+> x1 )) tp tp
     where tp =  (t4 ->- t1 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t4 ->- t1 ->- t2 ->- t

cSBC = CLeaf "SBC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2) <+> (g <+> x1 <+> x3 )) tp tp
     where tp = (t3 ->- t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t

cSCS = CLeaf "SCS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x3) <+> (g <+> x1 <+> x2 <+> x3 )) tp tp
     where tp = (t2 ->- t4 ->- t1 ->- t)
              ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cSCB = CLeaf "SCB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x3) <+> (g <+> x1 <+> x2  )) tp tp
     where tp = (t3 ->- t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t4 ->- t1 ->- t


cSCC = CLeaf "SCC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1) <+> (g <+> x1 <+> x2 <+> x3  )) tp tp
     where tp = (t2 ->- t1 ->- t) ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cBSS = CLeaf "BSS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g  <+> x2 <+> x3  )) tp tp
     where tp = (t1 ->- t3 ->- t4 ->- t2 ->- t)
              ->- (t3 ->- t4 ->- t2) ->- t1 ->- t3 ->- t4 ->- t

cBSB = CLeaf "BSB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3) <+> (g  <+> x2   )) tp tp
     where tp = (t1 ->- t4 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t1 ->- t4 ->- t2 ->- t

cBSC = CLeaf "BSC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 ) <+> (g  <+> x2 <+> x3   )) tp tp
     where tp = (t1 ->- t3 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t1 ->- t3 ->- t4 ->- t

cBBS = CLeaf "BBS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3 ) <+> (g  <+> x3   )) tp tp
     where tp = (t1 ->- t2 ->- t4 ->- t3 ->- t) ->- (t4 ->- t3) ->- t1 ->- t2 ->- t4 ->- t

cBBB = CLeaf "BBB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2 <+> x3 ) <+> g ) tp tp
     where tp = (t1 ->- t2 ->- t3 ->- t4 ->- t) ->- t4 ->- t1 ->- t2 ->- t3 ->- t


cBBC = CLeaf "BBC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x2  ) <+> (g <+> x3) ) tp tp
     where tp = (t1 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t1 ->- t2 ->- t4 ->- t

cBCS = CLeaf "BCS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x3  ) <+> (g <+> x2 <+> x3) ) tp tp
     where tp = (t1 ->- t4 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t1 ->- t3 ->- t4 ->- t

cBCB = CLeaf "BCB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1 <+> x3  ) <+> (g <+> x2) ) tp tp
     where tp = (t1 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t1 ->- t4 ->- t2 ->- t

cBCC = CLeaf "BCC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x1   ) <+> (g <+> x2 <+> x3) ) tp tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t1 ->- t3 ->- t4 ->- t

cCSS = CLeaf "CSS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 <+> x3   ) <+> (g <+> x1 <+> x2 <+> x3) ) tp tp
     where tp = (t3 ->- t4 ->- t1 ->- t)
              ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cCSB = CLeaf "CSB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 <+> x3   ) <+> (g <+> x1 <+> x2) ) tp tp
     where tp = (t4 ->- t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t4 ->- t1 ->- t

cCSC = CLeaf "CSC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2    ) <+> (g <+> x1 <+> x2 <+> x3) ) tp tp
     where tp = (t3 ->- t1 ->- t) ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cCBS = CLeaf "CBS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 <+> x3) <+> (g <+> x1  <+> x3) ) tp tp
     where tp = (t1 ->- t4 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t

cCBB = CLeaf "CBB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 <+> x3) <+> (g <+> x1) ) tp tp
     where tp = (t1 ->- t2 ->- t3 ->- t) ->- (t4 ->- t3) ->- t4 ->- t1 ->- t2 ->- t

cCBC = CLeaf "CBC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x2 ) <+> (g <+> x1 <+> x3) ) tp tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t

cCCS = CLeaf "CCS" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x3 ) <+> (g <+> x1 <+> x2 <+> x3) ) tp tp
     where tp = (t4 ->- t1 ->- t) ->- (t2 ->- t3 ->- t4 ->- t1) ->- t2 ->- t3 ->- t4 ->- t

cCCB = CLeaf "CCB" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f <+> x3 ) <+> (g <+> x1 <+> x2 ) ) tp tp
     where tp = (t1 ->- t2 ->- t) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t4 ->- t1 ->- t

cCCC = CLeaf "CCC" (Func $ \f ->
                Func $ \g -> 
                Func $ \x1 ->
                Func $ \x2 ->
                Func $ \x3 ->
                (f ) <+> (g <+> x1 <+> x2 <+> x3) ) tp tp
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