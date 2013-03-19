
import Control.DeepSeq
import Type
import StdLib
import Enumerate

out = map runTI $ enumCombsToProb  stdlibTrie (-3) 3 Rtype

main = do
  putStrLn $ "Finished. Total length " ++ show (length out)