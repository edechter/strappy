{-# LANGUAGE FlexibleInstances #-}


-- | Memory-efficient enumeration of programs
--   Enumerates in place by writing down bit strings in lexicographic order;
--   Each bit string is an encoded program

module Strappy.EnumBits where

import Strappy.Expr
import Strappy.Library
import Strappy.Type
import Strappy.Utils
import Strappy.EnumBF

import Control.Monad.State
import Control.Monad.Maybe
import Data.List
import Data.Function
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

data PrefixTree a = PrefixBranch (PrefixTree a) (PrefixTree a) | PrefixLeaf a

data Bit = Zero | One
         deriving(Eq, Ord)

instance Show Bit where
	show Zero = "0"
	show One = "1"

data ParseResult = ParseBlacklist | ParseFrontier

-- | Finds the next string of bits, in lexicographic order, that is not a prefix of an already parsed program
getNextBitstring :: PrefixTree ParseResult -> [Bit] -> [Bit]
getNextBitstring parses bits =
  let bits' = if all (==One) bits
              then {-trace ("Enumerated up to length " ++ show (length bits)) $-} replicate (length bits + 1) Zero
              else bitSucc bits
  in if isPrefixOfProgram parses bits'
     then getNextBitstring parses bits'
     else bits'
  where bitSucc (One:bs) = Zero : bitSucc bs
        bitSucc (Zero:bs) = One : bs
        isPrefixOfProgram (PrefixLeaf ParseFrontier) _ = False 
        isPrefixOfProgram (PrefixLeaf ParseBlacklist) _ = True
        isPrefixOfProgram (PrefixBranch l _) (Zero:bs) = isPrefixOfProgram l bs
        isPrefixOfProgram (PrefixBranch _ r) (One:bs) = isPrefixOfProgram r bs
        -- Should never occur, because we enumerate in order of length
        isPrefixOfProgram (PrefixBranch {}) [] = error "Attempt to try bit string shorter than existing strings"
  

enumBits :: Grammar -> Int -> Type -> [Expr]
enumBits (Grammar { grApp = logApp, grExprDistr = distr}) i tp =
  let tree = PrefixLeaf ParseFrontier
  in enum tree [] Set.empty
  where distr' = Map.toList distr
        enum _ _ s | Set.size s >= i = Set.toList s
        enum t bits s =
          case runTI (parseBits tp (logApp, distr') bits) of
            Nothing -> -- Parse failure
              enum t (getNextBitstring t bits) s
            Just (e,[]) -> -- Succeeded in producing a program: add to prefix tree
              let t' = blacklistPrefix t bits
              in enum t' (getNextBitstring t' bits) $ Set.insert e s
            -- Should never occur
            Just (_,_) -> error "Produced a longer program before a shorter one"
  
blacklistPrefix :: PrefixTree ParseResult -> [Bit] -> PrefixTree ParseResult
blacklistPrefix _ [] = PrefixLeaf ParseBlacklist
blacklistPrefix (PrefixLeaf ParseBlacklist) _ = (PrefixLeaf ParseBlacklist)
blacklistPrefix (PrefixLeaf ParseFrontier) (Zero:bs) =
	PrefixBranch (blacklistPrefix (PrefixLeaf ParseFrontier) bs) (PrefixLeaf ParseFrontier)
blacklistPrefix (PrefixLeaf ParseFrontier) (One:bs) =
	PrefixBranch (PrefixLeaf ParseFrontier) (blacklistPrefix (PrefixLeaf ParseFrontier) bs)
blacklistPrefix (PrefixBranch l r) (Zero:bs) = PrefixBranch (blacklistPrefix l bs) r
blacklistPrefix (PrefixBranch l r) (One:bs) = PrefixBranch l (blacklistPrefix r bs)


parseBits :: Type -> -- ^ Requested type
             (Double, [(Expr, Double)]) -> -- ^ Grammar we're enumerating from
             [Bit] -> -- ^ Encoded program
             TypeInference Maybe (Expr, [Bit]) -- ^ Parsed expression, remaining bits
parseBits _ _ [] = lift Nothing
parseBits reqTp gr@(logApp, distr) bits =
  -- Find all possible productions
  let prods = filter (\(e, _) -> canUnifyFast reqTp (eType e)) distr
      logZ = logSumExpList $ map snd prods
      -- Normalize and scale by probability of using a terminal
      logTerm = log (1-exp logApp)
      prods' = map (\(e, w) -> (Just e, w-logZ+logTerm)) prods
      -- Make prefix tree
      ptree = huffman $ [(Nothing, logApp)] ++ prods'
  in do
    (choice, bits') <- lift $ parseHuffman bits ptree
    case choice of
      Nothing -> do -- Application
        t <- mkTVar
        (f, bits'') <- parseBits (t ->- reqTp) gr bits'
        t' <- applySub t
        (a, bits''') <- parseBits t' gr bits''
        let e = f <> a
        return (e, bits''')
      Just term -> do -- Terminal
        tTp <- instantiateType (eType term)
        unify tTp reqTp
        return (term, bits')

unparseBits :: Type -> -- ^ Requested type
               Grammar -> -- ^ Library
               Expr -> -- ^ Expression to convert to bits
               [Bit] -- ^ Encoding of the program
unparseBits tp (Grammar { grApp = logApp, grExprDistr = distr }) expr =
  fromJust $ runTI $ encode tp expr
  where distr' = Map.toList distr
        encode :: Type -> Expr -> TypeInference Maybe [Bit]
        encode reqTp e = 
          -- Find all possible productions
          let prods = filter (\(p, _) -> canUnifyFast reqTp (eType p)) distr'
              logZ = logSumExpList $ map snd prods
              -- Normalize and scale by probability of using a terminal
              logTerm = log (1-exp logApp)
              prods' = map (\(p, w) -> (Just p, w-logZ+logTerm)) prods
              -- Make codebook
              book :: [(Maybe Expr, [Bit])]
              book = huffman2alist $ huffman $ [(Nothing, logApp)] ++ prods'
              -- Encoding of this choice point
              headChoice :: Maybe Expr
              headChoice = liftM fst $ find ((==e) . fst) prods
              encodingHead = fromJust $ lookup headChoice book
          in case headChoice of
               Just term -> do -- terminal
                 tTp <- instantiateType (eType term)
                 unify tTp reqTp
                 return encodingHead
               Nothing -> do -- application
                 t <- mkTVar
                 fBits <- encode (t ->- reqTp) (eLeft e)
                 t' <- applySub t
                 aBits <- encode t' (eRight e)
                 return $ encodingHead ++ fBits ++ aBits

-- | Uses huffman encoding to build a prefix code for the given (datum, log prob) pairs
huffman :: [(a, Double)] -> PrefixTree a
huffman [] = error "Attempt to create an empty Huffman encoding"
huffman datums = huffman' $ sortBy (compare `on` snd) $ map (\(d, l) -> (PrefixLeaf d, l)) datums
  where huffman' [(x,_)] = x
        huffman' ((x, pX):(y, pY):zs) = huffman' $ insertBy (compare `on` snd) (PrefixBranch x y, logSumExp pX pY) zs

-- | Picks out an element of a prefix tree based on a bit string
parseHuffman :: [Bit] -> PrefixTree a -> Maybe (a, [Bit])
parseHuffman bits (PrefixLeaf x) = return (x, bits)
parseHuffman (Zero:bits) (PrefixBranch l _) = parseHuffman bits l
parseHuffman (One:bits) (PrefixBranch _ r) = parseHuffman bits r
parseHuffman _ _ = Nothing

huffman2alist :: PrefixTree a -> [(a, [Bit])]
huffman2alist (PrefixLeaf a) = [(a, [])]
huffman2alist (PrefixBranch l r) =
	map (\(x,bs)->(x,Zero:bs)) (huffman2alist l)
	++ map (\(x,bs)->(x,One:bs)) (huffman2alist r)

