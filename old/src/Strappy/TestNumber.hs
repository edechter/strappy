

-- Author: Eyal Dechter
-- Date : 5/29/2014

--module Strappy.TestNumber where 

import Strappy.Number hiding (main)
import Strappy.EnumBF
import Strappy.EnumBits
import Strappy.Type
import Strappy.Library
import System.Environment

main = do
    args <- getArgs
    let enumType = head args
    let frontierSize = read $ args !! 1
    if enumType == "enumBF" then do
        putStrLn "Enumeration type: enumBF"
        print $ enumBF seedGrammar frontierSize tInt
                            else do 
        putStrLn "Enumeration type: enumBits"
        print $ enumBits seedGrammar frontierSize tInt

    putStrLn "Testing enumeration from seed grammar used in Number experiment."
