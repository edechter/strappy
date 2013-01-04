-- Grammar.hs

module Grammar where

import CL (show')
import qualified CombMap as CM
import CombMap (CombMap)


data Grammar = Grammar { library :: CombMap Int,
                         expandCost :: Int} 

showLibrary :: Show a => CombMap a  -> String
showLibrary ct = unlines $ map (\(c, i) -> show i ++ ": " ++ 
                               show' c) (CM.toList ct)

instance Show Grammar where
    show (Grammar lib c) = "Grammar\n------------------------" 
                           ++ "\nExpansions cost: " ++ show c
                           ++ "\n Library: " 
                           ++ showLibrary lib
    
    