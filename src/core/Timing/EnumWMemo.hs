
import Control.DeepSeq
import Type
import StdLib
import Enumerate

out = map runTI $ enumCombsToProb stdlibTrie (-200) 4 Rtype

main = do
  putStrLn $ --(any (==(head out)) out) `seq` 
               "Finished. Total length " ++ show (length out)