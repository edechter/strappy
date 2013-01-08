-- TestDriver.hs


import TestType(typeTestGroup)
import TestGrammar(grammarTestGroup)
import TestCompress(compressTestGroup)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List


main = defaultMain tests

tests = [typeTestGroup, 
         grammarTestGroup,
         compressTestGroup
        ]
    
