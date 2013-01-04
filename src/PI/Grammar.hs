-- Grammar.hs

module Grammar where

import CL 
import qualified CombMap as CM
import CombMap (CombMap)
import Compress


data Grammar = Grammar { library :: CombMap Int,
                         expansions :: Int} 

showLibrary :: Show a => CombMap a  -> String
showLibrary ct = unlines $ map (\(c, i) -> show i ++ ": " ++ 
                               show' c) (CM.toList ct)

instance Show Grammar where
    show (Grammar lib c) = "Grammar\n------------------------" 
                           ++ "\nExpansions: " ++ show c
                           ++ "\n Library: " 
                           ++ showLibrary lib

estimateGrammar :: [Comb] -> Grammar
-- | Generate a new grammar from a set of combinators using the
-- combinator tree compression scheme defined in Compress.hs
-- (compress). Include all trees from the compression that occur more
-- than once (i.e. Sequitur / Neville-Manning algorithm). To estimate
-- the expansions, take the total number of trees in from the
-- compression and subtract the number of input combinators.
estimateGrammar cs = Grammar lib c
    where ind = compress cs
          xs = CM.assocs ind
          count = CM.fold (+) 0 ind -- sum of tree counts in index
          lib = CM.fromList $ filter ((>1) . snd) xs 
          c = count - length cs
          
    
    