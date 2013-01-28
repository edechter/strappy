-- BooleanCircuit


module BooleanCircuits where

import Control.Monad
import Control.Monad.Random
import qualified Data.Set as Set
import qualified Data.List.Split as Split
import qualified Data.List as List
import Debug.Trace

import DigArith
import Task


data MonoGate = MonoGate {monoGateName :: String, 
                          monoGateFunc :: Bool -> Bool}

data BinGate = BinGate {binGateName :: String, 
                        binGateFunc :: Bool -> Bool -> Bool}

data Const = Const {constVal :: Bool}

data Gate = MGate MonoGate | BGate BinGate | CGate Const deriving Show
          

type GateId = Int
data GateInstance = MGateInst {gateId :: GateId, 
                               mgate :: MonoGate,
                               input :: GateId}
                  | BGateInst {gateId :: GateId,
                               bgate :: BinGate,
                               input1 :: GateId,
                               input2 :: GateId}
                  | ConstInst {gateId :: GateId, 
                              cgate :: Const}

getInputs :: GateInstance -> [GateId]
getInputs MGateInst{input=i} = [i]
getInputs BGateInst{input1=i1, input2=i2} = [i1, i2]
getInputs ConstInst{} = []




instance Show MonoGate where
    show (MonoGate name _) = name
instance Show BinGate where
    show (BinGate name _) = name
instance Show Const where
    show (Const b) = show b
instance Show GateInstance where
    show (MGateInst id g i) = show id ++"-" ++  show g ++ "-" ++ show i
    show (BGateInst id g i1 i2 ) = show id ++ "-" ++ show g ++ "-" ++  "(" ++
                                   show i1 ++ ", " ++ show i2 ++ ")"
    show (ConstInst id c) = "x" ++ show id ++ ": " ++ show c
                               
data Circuit = Circuit {circuitGates :: [GateInstance]} deriving Show

getNewId (Circuit gs) = (maximum $ map gateId gs) + 1

findGateById (Circuit (g:gs)) id | gateId g == id = Just g
                                 | otherwise = findGateById (Circuit gs) id
findGateById (Circuit []) id = Nothing

setConstValueById :: Circuit -> Int -> Bool -> Circuit
setConstValueById (Circuit []) id val = Circuit []
setConstValueById (Circuit (g:gs)) id val 
    | gateId g == id = Circuit ((ConstInst id (Const val)):gs)
    | otherwise = Circuit (g:gs')
    where (Circuit gs') = setConstValueById (Circuit gs) id val

circuitCardinality :: Circuit -> Int
circuitCardinality (Circuit gs) = length $ filter isConstInst gs
    where isConstInst ConstInst{} = True
          isConstInst _ = False


probeGate :: Circuit -> GateInstance -> Maybe Bool
probeGate circuit (MGateInst id g inpId)  
    = do inpGate <- findGateById circuit inpId
         inpVal <- probeGate circuit inpGate
         return $ (monoGateFunc g) inpVal
probeGate circuit (BGateInst id g inpId1 inpId2)  
    = do inpGate1 <- findGateById circuit inpId1
         inpGate2 <- findGateById circuit inpId2
         inpVal1 <- probeGate circuit inpGate1
         inpVal2 <- probeGate circuit inpGate2
         return $ (binGateFunc g) inpVal1 inpVal2
probeGate circuit (ConstInst id g)
    = return (constVal g)

probeCircuit :: Circuit -> Maybe Bool
probeCircuit circ@(Circuit (g:gs)) = probeGate circ g

probeCircuitOnTuple :: Circuit -> [Bool] -> Maybe Bool
probeCircuitOnTuple circuit tuple = probeCircuit circuit'
    where circuit' = setInputValsToTuple circuit tuple
    



------- Canonical Gates --------
notGate = MonoGate "NOT" not
andGate = BinGate "AND" (&&)
orGate = BinGate "OR" (||)
xorGate = BinGate "XOR" (\x y -> (x && not y) || (y && not x))
nandGate = BinGate "NAND" (\x y -> not (x && y))
true = Const True
false = Const True
--------------------------------

-- Create random circuits
-- 1) choose distribution of gates
-- 2) choose n gates randomly from this distribution
-- 3) in order
------ a) for each output of a gate choose a random input downstream
------ b) continue until finished

numGatesDistribution :: [(Int, Rational)]
numGatesDistribution = [(1, 1), (2, 2), (3, 3), (4, 2)] 

gateDistribution :: [(Gate, Rational)]
gateDistribution = [ (MGate notGate, 1), (BGate andGate, 2)]

sampleNumGates :: MonadRandom m => m Int
sampleNumGates = fromList numGatesDistribution

sampleGate :: MonadRandom m => m Gate
sampleGate = fromList gateDistribution

sampleGatesFromDistribution :: MonadRandom m =>
                               [(Gate, Rational)]
                            -> Int -- ^ number of gates
                            -> m [Gate]
sampleGatesFromDistribution dist n = sequence $ replicate n (fromList dist)

addInst :: MonadRandom m => Circuit -> Gate -> m Circuit
addInst circ@(Circuit gs) (MGate m) = 
    do inputGateInst <- fromList $ zip gs (repeat 1) -- choose a previous gate uniformly
       let newInst = MGateInst newId m (gateId inputGateInst)
       return $ Circuit (newInst:gs)
    where newId = getNewId circ
addInst circ@(Circuit gs) (BGate m) = 
    do inputGateInst1 <- fromList $ zip gs (repeat 1) -- choose a previous gate uniformly
       inputGateInst2 <- fromList $ zip gs (repeat 1) -- choose a previous gate uniformly
       let newInst = BGateInst newId m 
                     (gateId inputGateInst1)
                     (gateId inputGateInst2)
       return $ Circuit (newInst:gs)
    where newId = getNewId circ
addInst circ@(Circuit gs) (CGate m) = 
    do let newInst = ConstInst newId m 
       return $ Circuit (newInst:gs)
    where newId = getNewId circ

addInstances :: MonadRandom m => Circuit -> [Gate] -> m Circuit
addInstances circ gs = foldM addInst circ gs

createInputs :: Int -> Circuit
createInputs n = Circuit [(ConstInst i true) | i <- [0..(n-1)]]


setInputValsToTuple :: Circuit
                    -> [Bool] -- ^ input values (must be same length
                              -- as number of inputs)
                    -> Circuit
setInputValsToTuple circ vals = foldl (\cr (i,v) -> setConstValueById cr i v)
                                circ
                                (zip [0..] vals)

sampleCircuitNGates :: MonadRandom m 
                    => Int -- ^ number of inputs
                    -> Int -- ^ number of gates
                    -> [(Gate, Rational)] -- ^ gate distribution
                    -> m Circuit
sampleCircuitNGates nI nG distr = 
    do gates <- sampleGatesFromDistribution distr nG
       let circ = createInputs nI
       addInstances circ gates

isCircuitConnected :: Circuit -> Bool
-- | A circuit is connected if every output except for the last one is
-- an input for some other gate
isCircuitConnected circ =  (Set.size diff == 1) && (diff == Set.singleton outId)
    where allInputs = Set.fromList $ join $ map getInputs $ circuitGates circ
          allGateIds = Set.fromList $ map gateId $ circuitGates circ
          diff = Set.difference  allGateIds allInputs
          outId = gateId $ head $ circuitGates circ

sampleConnectedCircuitGivenNumbers :: MonadRandom m 
                             => Int -- ^ number of inputs
                          -> Int -- ^ number of gates
                          -> [(Gate, Rational)]
                          -> m Circuit
sampleConnectedCircuitGivenNumbers nI nN dG 
    = do gates <- sampleGatesFromDistribution dG nN
         let circ = createInputs nI
         circ' <- addInstances circ gates
         if isCircuitConnected circ' then
             return circ' else
             sampleConnectedCircuitGivenNumbers nI nN dG
                             

sampleConnectedCircuit dI dN dG 
    = do nI <- fromList dI
         nN <- fromList dN
         if nI > 2 && nN==1 then
             sampleConnectedCircuit dI dN dG else
              sampleConnectedCircuitGivenNumbers nI nN dG         


         
sampleConnectedCircuits dI dN dG nC = sequence $ replicate nC 
                                      $ sampleConnectedCircuit dI dN dG



circuitToTruthTable :: Circuit -> Maybe TruthTable
circuitToTruthTable circuit  
    = do vals <- sequence [probeCircuitOnTuple circuit tuple |  tuple <- tuples]
         return $ zip tuples vals         
    where tuples = mkAllAss $ circuitCardinality circuit

circuitToTask :: String -> Circuit -> Maybe Task
circuitToTask taskname circuit = do tt <- circuitToTruthTable circuit
                                    return $ mkBoolTask taskname tt

sampleCircuitTasks dI dN dG nT 
    = do circuits <- sampleConnectedCircuits dI dN dG nT 
         return (map (\(i, c) -> (c,  fromJust $ circuitToTask ("circuitTask_" ++ show i) c))
                    $ zip [0..] circuits)
    where fromJust (Just x) = x


equiv :: Circuit -> Circuit -> Bool
equiv c1 c2 = tt1 == tt2
      where tt1 = circuitToTruthTable c1
            tt2 = circuitToTruthTable c2

mkEquivClasses :: [Circuit] -> [[Circuit]]
mkEquivClasses cs = foldl mkEquivClasses' [] cs

mkEquivClasses' :: [[Circuit]] -> Circuit -> [[Circuit]]
mkEquivClasses' [] c = [[c]]
mkEquivClasses' (cl:cls) c = if (head cl) `equiv` c then
                                 (c:cl):cls else
                                 (cl):(mkEquivClasses' cls c)
                                     
       
          

-- DISTRIBUTIONS -- 
dNumInputs = [(1, 1), (2, 2), (3, 4)] :: [(Int, Rational)]
dNumGates = [(1, 1), (2, 3), (3,  4), (4, 5), (5, 4)] :: [(Int, Rational)]
dGateType = [ (MGate notGate, 1), (BGate andGate, 2), (BGate orGate, 2)] :: [(Gate, Rational)]





                                               
                                               

-- not1 = MGateInst 2 notGate 1
-- x = ConstInst 1 (Const True)
-- circuit = Circuit [not1, x]

      
--- Saving circuits to file ---

writeGateInstance :: GateInstance -> String
writeGateInstance (MGateInst id gate inp) = "mono " 
                                       ++ show id ++ " " 
                                       ++ show gate ++ " "
                                       ++ show inp
writeGateInstance (BGateInst id gate inp1 inp2) = "binary " ++ 
                                             show id ++ " " 
                                             ++ show gate ++ " "
                                             ++ show inp1 ++ " "
                                             ++ show inp2 
writeGateInstance (ConstInst id _) = "const " ++ show id

writeCircuit :: Circuit -> String
writeCircuit (Circuit gs) = "circuit start\n" 
                            ++ unlines (map writeGateInstance gs)
                            ++ "circuit end"

writeCircuitTask :: (Circuit, Task) -> String
writeCircuitTask (c, t) = "task name " ++ show t ++ "\n" 
                          ++ writeCircuit c

writeCircuitTasks :: [(Circuit, Task)] -> String
writeCircuitTasks xs = unlines $ map (\x -> writeCircuitTask x ++ "\n")
                       xs

writeCircuitTasksToFile :: [(Circuit, Task)] -> String -> IO ()
writeCircuitTasksToFile xs file  = writeFile file $ writeCircuitTasks xs

readCircuitTasksFile :: String -- ^ filename
                        -> IO [(Circuit, String)]
readCircuitTasksFile filepath = do 
  str <- readFile filepath
  return $ readCircuitTasks str

readCircuitTasks :: String -> [(Circuit, String)]
readCircuitTasks xs = map readCircuitTask $ filter (not . null) $ Split.splitOn "\n\n" xs

readCircuitTask str = (Circuit instances, taskName)
    where (l:ls) = lines str
          (Just taskName) = List.stripPrefix "task name " l
          instances = map fromJust $ filter isJust $ map process ls
          process x = listToInstance (words x) 
          isJust (Just x) = True
          isJust Nothing = False
          fromJust (Just x) = x
          
listToInstance :: [String] -> Maybe GateInstance
listToInstance [_, outIdstr, "NOT", inIdstr] = Just (MGateInst outId notGate inId)
                                                where inId = read inIdstr
                                                      outId = read outIdstr
listToInstance [_, outIdstr, "AND", inId1str, inId2str] = Just (BGateInst outId andGate inId1 inId2)
     where inId1 = read inId1str
           inId2 = read inId2str
           outId = read outIdstr
listToInstance [_, outIdstr, "OR", inId1str, inId2str] = Just (BGateInst outId orGate inId1 inId2)
    where inId1 = read inId1str
          inId2 = read inId2str
          outId = read outIdstr
listToInstance ["const", outIdstr] = Just (ConstInst (read outIdstr) true)
listToInstance _ = Nothing 

readTasksFromCircuits :: String -- ^ filename
                      -> IO [Task]
readTasksFromCircuits filename = do
  xs <- readCircuitTasksFile filename
  return $ map (\(c, n) -> fromJust (circuitToTask n c)) xs
    where fromJust (Just x) = x


         
                  


                      