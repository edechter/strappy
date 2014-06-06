module Strappy.Response
(Response(..),
 getNod,
 getGive,
 getSpeak
) where

data Response =
    Nod Bool   |
    Give [Int] |
    Speak [Char]
    deriving (Show)

getNod :: Response -> Maybe Bool
getNod (Nod b) = Just b
getNod _       = Nothing

getGive :: Response -> Maybe [Int]
getGive (Give xs) = Just xs
getGive _         = Nothing

getSpeak :: Response -> Maybe [Char]
getSpeak (Speak xs) = Just xs 
getSpeak _          = Nothing
