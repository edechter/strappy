-- TestListTI.hs

import ListTDoneRight

import Data.Maybe
import Type
import qualified Data.Map as Map
import Debug.Trace
import Control.Monad
import Control.Monad.Trans

import StdLib (stdlib)
import StdLib
import CL

lib = Map.elems stdlib

-- type ListTI = ListT TI 


-- enum :: [Comb] -> Int -> Type -> ListTI Comb
-- enum lib 0 t = filterCombinatorsByType' lib t
-- enum lib d t = rest `mplus` enum lib 0 t 
--     where rest = do 
--             tp <- lift $ newTVar Star
--             v <- lift $ getVarInt
--             a <- enum lib (d-1) (tp ->- t)
--             aType <- lift $ typeCheck a
--             b <- enum lib (d-1) (tp)
--             let c = CApp a b [] (mkAppDepth a b )
--             (trace $ 
--              "\n INFO: " ++
-- --             "\n depth: " ++ show d ++ 
--              "\n varInt: " ++ show v ++
--              "\n comb: " ++ show a ++ 
--              "\n inType: " ++ show (tp ->- t) ++
--              "\n outType: " ++ show (aType)
--                    )  $ return c
            


-- -- | Filter combinators by their types, using unification
-- filterCombinatorsByType' :: [Comb] -> Type -> ListTI Comb
-- filterCombinatorsByType' xs t 
--     = do c <- liftList  xs
--          c' <- lift $ freshInstComb c
--          tc <- lift $ typeCheck c'
--          succ <- lift $ unify' tc t
--          return c' 

      
-- toList :: ListTI a -> [a]
-- toList x = case (runTISafe $ runListT x) of
--              Right (Just y) -> ((fst y) : toList (snd y))
--              Right (Nothing) -> []

-- newtype AmbTI a = AmbTI { runAmbTI :: Maybe ( TI a, AmbTI a) }

-- headAmbTI :: AmbTI a 

-- instance Monad AmbTI where
--     return x = AmbTI (Just (return x, mzero))
-- --     (AmbTI (Just (t1, (AmbTI Nothing))) >>= k -- a -> AmbTI b
-- --            = AmbTI $ Just (h, rest)
-- --              where h = do a <- t1
-- --                           return $ k a
-- --     (AmbTI (Just (t1, rest))) >>= k
-- --             = let f 
-- --         l2 = (t1 >>= k)
-- --               in l2 `mplus` (rest >>= k)    
                     
-- instance MonadPlus AmbTI where
--     mzero = AmbTI Nothing
--     AmbTI l1 `mplus` AmbTI l2 = AmbTI $
--           case l1 of
--             (Just (t1, rest)) -> Just (t1, rest `mplus` (AmbTI l2))
--             otherwise -> l2

newtype AmbTI a = AmbTI { runAmbTI :: [TI a] }

collapseBranch :: TI (AmbTI a) -> AmbTI a
collapseBranch m = case runTISafe m of
      Right (AmbTI result) -> AmbTI [ m >>  t  | t <- result]
      Left err -> AmbTI [throwTIError err ]
                         
                                        

instance Monad AmbTI where
    return x = AmbTI [return x]
    (AmbTI (x:xs)) >>= k 
        = let first = collapseBranch $ do
                        a <- x 
                        return $ k a
              rest = (AmbTI xs) >>= k
          in first `mplus` rest
    (AmbTI []) >>= k = (AmbTI [])

instance MonadPlus AmbTI where
    mzero = AmbTI []
    (AmbTI a) `mplus` (AmbTI b)  = AmbTI (a ++ b)
      
liftTI :: TI a -> AmbTI a
liftTI t = AmbTI [t]

enum :: [Comb] -> Int -> Type -> AmbTI Comb
enum lib 0 t = filterCombinatorsByType' lib t
enum lib d t = enum lib 0 t `mplus` rest
    where rest = do 
            tp <- liftTI $ newTVar Star
            v <- liftTI $ getVarInt
            a <- enum lib (d-1) (tp ->- t)
            aType <- liftTI $ typeCheck a
            b <- enum lib (d-1) (fromType aType)
            let c = CApp a b [] (mkAppDepth a b )
            s <- liftTI $ getSubst
            (trace $ 
             "\n INFO: " ++
             "\n depth: " ++ show d ++ 
             "\n varInt: " ++ show v ++
             "\n sub: " ++ show s ++                           
             "\n comb a: " ++ show a ++ 
             "\n comb b: " ++ show b ++ 
             "\n inType: " ++ show (tp ->- t) ++
             "\n outType: " ++ show (aType)
                   )  $ liftTI $ typeCheck c
            return c

-- | Filter combinators by their types, using unification
filterCombinatorsByType' :: [Comb] -> Type -> AmbTI Comb
filterCombinatorsByType' xs t  
    = do c <- AmbTI ( map return xs)
         c' <- liftTI $ freshInstComb c
         tc <- liftTI $ typeCheck c'
         s <- liftTI $ getVarInt
         subst <- liftTI $ getSubst
         succ <- (trace $ show t ++ " " ++ show tc ++ " " ++ show subst) 
                 $  liftTI $ unify' tc t
         (trace $ "succ : " ++ show succ ) $ return c

      
toList :: AmbTI a -> [a]
toList (AmbTI (x:xs)) = case runTISafe x of 
                        Right a -> (a:rest)
                        Left err -> (rest)
    where rest = toList (AmbTI xs)
toList (AmbTI []) = []

    

a = newTVar Star
b = newTVar Star
c = (newTVar Star) >> (return $AmbTI [a, b])
out = toList $ collapseBranch c

