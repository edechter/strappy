-- Expr.hs
-- |
-- Module:      Strappy.Core.Expr
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- Types and methods for our expression language.
--
-- Expressions are either primitives, constructed with the @Term@
-- constructor, or they are binary applications of expressions,
-- constructed using the @Expr@ constructor. Expressions are typed,
-- using the types defined in @Strappy.Type@.

module Strappy.Expr (
  -- * Types
  Expr(..),
  -- * Constructors
  (<>),
  (<.>),
  -- * Evaluation
  eval,
  timeLimitedEval,
  -- * Type inference
  unifyExpr,
  exprUnifies,
  typeOfApp,  
  doTypeInference,
  doTypeInferenceM,
  typeChecks,
  -- * Misc
  isTerm,  
  exprSize,
  getArity,
  subExpr
  ) where


-- External Imports --------
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad
import Data.Hashable
import Control.Exception
import Control.Monad.Error.Class
import System.IO.Unsafe
import Control.Concurrent.Timeout (timeout)
import Data.Timeout (Timeout(..))
import Data.String (IsString)
import Criterion (nf, run)
import Data.Word

-- Strappy imports --------
import Strappy.Type
import Numeric.StatsUtils

-- | Main data type for expressions. 
data Expr = forall a.
            Term {eName  :: String, 
                  eType  :: Type, 
                  eThing :: a      -- ^ the haskell term of any type
                                   -- that is actually computed when
                                   -- this term is called
                 }
          | App {eLeft  :: Expr,
                 eRight :: Expr,
                 eType  :: Type}
             
-- | Smart constructor for non-monadic applications.
(<>) :: Expr -> Expr -> Expr
a <> b = App { eLeft  = a, 
               eRight = b, 
               eType  = tp
             }
          where tp = case evalTI $ typeOfApp a b of
                      Left err  -> error err
                      Right t -> t

-- | infix constructor of expr applications within TypeInference monad.
(<.>) :: (IsString e, MonadError e m) => TypeInference m Expr -> TypeInference m Expr -> TypeInference m Expr
ma <.> mb = do a <- ma
               b <- mb
               ta <- instantiateType (eType a)
               tb <- instantiateType (eType b)
               let a' = a{eType=ta}
                   b' = b{eType=tb}
               tp <- typeOfApp a' b'
               return App{eLeft = a', eRight = b', eType = tp}
 
instance Show Expr where
    show Term{eName=s} = s
    show App{eLeft=el, eRight=er} = "(" ++ show el ++ " " ++  show er ++ ")"

instance Eq Expr where
  Term { eName = n } == Term { eName = n' } = n == n'
  App { eLeft = l, eRight = r} == App { eLeft = l', eRight = r' } = l == l' && r == r'
  _ == _ = False

instance Ord Expr where
    compare (Term {eName = n}) (Term {eName = n'}) = compare n n' 
    compare (App {}) (Term {}) = LT
    compare (Term {}) (App {}) = GT
    compare (App { eLeft = l, eRight = r }) (App { eLeft = l', eRight = r' }) =
      case compare l l' of
        EQ -> compare r r'
        cmp -> cmp

-- | @typeOfApp e1 e2@ returns the type of (App e1 e2) in in the current environment. 
typeOfApp :: (IsString e, MonadError e m) => Expr -> Expr -> TypeInference m Type
typeOfApp e_left e_right 
    = do tp <- mkTVar
         unify (eType e_left) (eType e_right ->- tp)
         applySub tp
         
-- | Evaluates an expression of type a into a Haskell object of that
-- corresponding type.
eval :: Expr -> a
eval Term{eThing=f} = unsafeCoerce f
eval App{eLeft=el, eRight=er} = eval el (eval er)

isTerm :: Expr -> Bool
isTerm Term{} = True
isTerm _ = False

-- | Evaluate an expression to a value, forcing the value to be
-- evaluated to normal form. Limit evaluation time to timeout
-- specified in nanoseconds.
timeLimitedEval :: Show a => Word64 -> Expr -> Maybe a
timeLimitedEval nanoSecs expr = unsafePerformIO $
                       handle (\(_ :: SomeException) -> return Nothing) $ 
                         timeout (Timeout nanoSecs) $ do
                           -- Hack to force Haskell to evaluate the expression:
                           -- Convert the (eval expr) in to a string,
                           -- then force each character of the string by putting it in to an unboxed array
                           let val = eval expr
                           run (nf show val) 1
                           return val


-- | Runs type inference on the given expression, returning its
-- type. This completely recalculates from the primitives.
doTypeInference :: (IsString e, MonadError e m) => Expr -> m Type
doTypeInference expr = evalTI $ doTypeInferenceM expr

doTypeInference_ expr = case doTypeInference expr of 
    Right t -> t
    Left err -> error err

-- | Like @doTypeInference@, but remain within the TypeInference monad
doTypeInferenceM :: (IsString e, MonadError e m) => Expr -> TypeInference m Type
doTypeInferenceM (Term { eType = tp }) = instantiateType tp
doTypeInferenceM (App { eLeft = l, eRight = r }) = do
  alpha <- mkTVar
  beta <- mkTVar
  lTp <- doTypeInferenceM l
  unify lTp (alpha ->- beta)
  rTp <- doTypeInferenceM r
  unify rTp alpha
  applySub beta


-- | Unify an expressions with a type in the TypeInference monad. 
unifyExpr :: (MonadError String m) => Type -> Expr -> TypeInference m ()
{-# INLINE unifyExpr #-}
unifyExpr tp expr = do 
    eTp <- instantiateType (eType expr)
    unify tp eTp
    

-- | Return true if expression and type can be unified (in empty context). 
exprUnifies :: Expr -> Type -> Bool
exprUnifies e t = fromEither $ evalTI $ catchError (f >> return True) (const $ return False)
  where f = do eTp <- doTypeInferenceM e
               unify t eTp
        fromEither (Right x) = x
        fromEither (Left err) = error $ "exprUnifies: the impossible happened. exprUnifies returned an err: " ++ err
               
-- | Return true if the expression typechecks in a clean
-- environment. Otherwise, false.
typeChecks :: Expr -> Bool
typeChecks expr = case evalTI $ doTypeInferenceM expr of 
        Left _ -> False
        Right _ -> True

-- TODO: Defined Foldable instance for Expr
-- | Folds a monadic procedure over each subtree of a given expression
exprFoldM :: Monad m => (a -> Expr -> m a) -> a -> Expr -> m a
exprFoldM f a e@(Term {}) = f a e
exprFoldM f a e@(App { eLeft = l, eRight = r}) = do
  a'   <- f a e
  a''  <- exprFoldM f a' l
  exprFoldM f a'' r

-- | Returns number of characters in an expression
-- | Does not count both opening and closing parens (these are redundant; see, for example, Unlambda)
exprSize :: Expr -> Int
exprSize Term {} = 1
exprSize App { eLeft = l, eRight = r } = 1 + exprSize l + exprSize r

-- | Return the number of arguments for the expression.
getArity :: (IsString e, MonadError e m) =>  Expr -> m Int
getArity expr =
  liftM arity $ doTypeInference expr
  where arity (TCon "->" [t1, t2]) = 1 + arity t2
        arity _ = 0

-- | Substitute instances of Old for New in Target
subExpr :: Expr -- ^ Old
        -> Expr -- ^ New
        -> Expr -- ^ Target
        -> Expr -- ^ Updated target
subExpr _ _ e@(Term { }) = e
subExpr old new target | target == old = new
subExpr old new e@(App { eLeft = l, eRight = r }) =
  e { eLeft = subExpr old new l,
      eRight = subExpr old new r }
  
----------------------------------------
-- HOLES  ------------------------------
----------------------------------------

-- TODO: Holes should have their own constructors
-- | Procedures for managing holes:
-- | Returns the number of holes in an expression
countHoles :: Expr -> Int
countHoles (App { eLeft = l, eRight = r }) = countHoles l + countHoles r
countHoles (Term { eName = "H" }) = 1
countHoles (Term {}) = 0

----------------------------------------
-- Conversion functions ----------------
----------------------------------------

showableToExpr :: (Show a) => a -> Type -> Expr
-- | Convert any Showable Haskell object into an Expr.
showableToExpr f tp = Term (show f) tp f

intListToExpr :: [Int] -> Expr
intListToExpr s = showableToExpr s (tList tInt)

stringToExpr :: String -> Expr
stringToExpr s = showableToExpr s (tList tChar)

doubleToExpr :: Double -> Expr
doubleToExpr d = showableToExpr d tDouble

charToExpr :: Char -> Expr
charToExpr c = showableToExpr c tChar

boolToExpr :: Bool -> Expr
boolToExpr b = showableToExpr b tBool


----------------------------------------
-- Hashable instance ------------------- 
----------------------------------------
instance Hashable Expr where
    hashWithSalt a (Term { eName = name }) = hash a `hashWithSalt` hash name                                                   
    hashWithSalt a (App { eLeft = left, eRight = right }) = 
      hash a `hashWithSalt` hash left `hashWithSalt` hash right




