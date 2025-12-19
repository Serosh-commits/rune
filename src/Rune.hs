{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rune where

import Prelude hiding (lookup)
import qualified Prelude as P

import Control.DeepSeq
import Control.Monad (guard)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortBy, maximumBy)
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import GHC.Exts (Any, unsafeCoerce)

newtype Score = Score { unScore :: Double }
  deriving (Show, Eq, Ord)

data FeatureType = FNum | FCat
  deriving (Show, Eq, Enum)

instance NFData FeatureType where
  rnf !_ = ()

data Metadata = Metadata
  { metaType :: !FeatureType
  , metaName :: !ByteString
  } deriving (Show, Eq)

instance NFData Metadata where
  rnf (Metadata t n) = rnf t `seq` rnf n

data FeatureVector = FeatureVector
  { fvData :: !(Vector Any)
  , fvMeta :: !(IntMap Metadata)
  }

instance Show FeatureVector where
  show _ = "<feature_vector>"

instance NFData FeatureVector where
  rnf (FeatureVector d m) = d `seq` rnf m

getNum :: FeatureVector -> Int -> Double
getNum fv i = unsafeCoerce (V.unsafeIndex (fvData fv) i)
{-# INLINE getNum #-}

getCat :: FeatureVector -> Int -> Int
getCat fv i = unsafeCoerce (V.unsafeIndex (fvData fv) i)
{-# INLINE getCat #-}

data Predicate where
  IsEq     :: !Int -> !Int -> Predicate
  IsGt     :: !Int -> !Double -> Predicate
  IsLt     :: !Int -> !Double -> Predicate
  InRange  :: !Int -> !Double -> !Double -> Predicate
  Oblique  :: !(VU.Vector Double) -> !Double -> Predicate
  And      :: !Predicate -> !Predicate -> Predicate
  Or       :: !Predicate -> !Predicate -> Predicate
  Not      :: !Predicate -> Predicate
  TruePred :: Predicate

instance Show Predicate where
  show (IsEq i v) = "x[" ++ show i ++ "] == " ++ show v
  show (IsGt i v) = "x[" ++ show i ++ "] > "  ++ show v
  show (IsLt i v) = "x[" ++ show i ++ "] < "  ++ show v
  show (InRange i l h) = show l ++ " < x[" ++ show i ++ "] < " ++ show h
  show (Oblique _ _) = "oblique"
  show (And a b) = "(" ++ show a ++ " && " ++ show b ++ ")"
  show (Or a b)  = "(" ++ show a ++ " || " ++ show b ++ ")"
  show (Not a)   = "!(" ++ show a ++ ")"
  show TruePred  = "true"

instance NFData Predicate where
  rnf (IsEq i v) = rnf i `seq` rnf v
  rnf (IsGt i v) = rnf i `seq` rnf v
  rnf (IsLt i v) = rnf i `seq` rnf v
  rnf (InRange i l h) = rnf i `seq` rnf l `seq` rnf h
  rnf (Oblique v t) = rnf v `seq` rnf t
  rnf (And a b) = rnf a `seq` rnf b
  rnf (Or a b)  = rnf a `seq` rnf b
  rnf (Not a)   = rnf a
  rnf TruePred  = ()

data ActionRep
  = ClassScore !Int !Double
  | Regress !Double
  deriving (Show, Eq)

instance NFData ActionRep where
  rnf (ClassScore c s) = rnf c `seq` rnf s
  rnf (Regress v) = rnf v

type Action = FeatureVector -> Score

evalAction :: ActionRep -> Action
evalAction (ClassScore _ s) _ = Score s
evalAction (Regress v) _ = Score v
{-# INLINE evalAction #-}

data Rule = Rule
  { ruleFP   :: !Word64
  , rulePred :: !Predicate
  , ruleAct  :: !ActionRep
  , ruleWt   :: !Double
  , ruleCov  :: !Int
  , ruleCor  :: !Int
  , ruleGen  :: !Int
  } deriving (Show)

instance NFData Rule where
  rnf (Rule f p a w c r g) =
    rnf f `seq` rnf p `seq` rnf a `seq`
    rnf w `seq` rnf c `seq` rnf r `seq` rnf g

newtype RuleSet = RuleSet
  { rules :: HM.HashMap Word64 Rule
  } deriving (Show)

instance NFData RuleSet where
  rnf (RuleSet r) = rnf r

hash64 :: Hashable a => a -> Word64
hash64 = fromIntegral . hash
{-# INLINE hash64 #-}

rotate64 :: Word64 -> Int -> Word64
rotate64 x k = (x `shiftL` k) .|. (x `shiftR` (64 - k))
{-# INLINE rotate64 #-}

hashPred :: Predicate -> Word64
hashPred (IsEq i v) = hash64 i `xor` hash64 v `xor` 0x12345678
hashPred (IsGt i v) = hash64 i `xor` hash64 v `xor` 0x87654321
hashPred (IsLt i v) = hash64 i `xor` hash64 v `xor` 0xDEADBEEF
hashPred (InRange i l h) = hash64 i `xor` hash64 l `xor` hash64 h `xor` 0xCAFEBABE
hashPred (Oblique v t) = hash64 (VU.toList v) `xor` hash64 t
hashPred (And a b) = hashPred a `xor` rotate64 (hashPred b) 13
hashPred (Or a b)  = hashPred a `xor` rotate64 (hashPred b) 19
hashPred (Not a)   = hashPred a `xor` maxBound
hashPred TruePred  = 0

matches :: Predicate -> FeatureVector -> Bool
matches (IsEq i v) fv = getCat fv i == v
matches (IsGt i v) fv = getNum fv i > v
matches (IsLt i v) fv = getNum fv i < v
matches (InRange i l h) fv =
  let x = getNum fv i in x > l && x < h
matches (Oblique w t) fv =
  let s = VU.ifoldl' (\a j c -> a + c * getNum fv j) 0 w
  in s > t
matches (And a b) fv = matches a fv && matches b fv
matches (Or a b)  fv = matches a fv || matches b fv
matches (Not a)   fv = not (matches a fv)
matches TruePred  _  = True

infer :: RuleSet -> FeatureVector -> Int
infer (RuleSet rs) fv =
  let acc = foldl' (tally fv) IM.empty (HM.elems rs)
  in if IM.null acc then 0 else fst (maximumBy (comparing snd) (IM.toList acc))

tally :: FeatureVector -> IntMap Double -> Rule -> IntMap Double
tally fv acc r
  | matches (rulePred r) fv =
      let Score s = evalAction (ruleAct r) fv
          conf = (fromIntegral (ruleCor r) + 1) /
                 (fromIntegral (ruleCov r) + 2)
          v = s * ruleWt r * conf
          cls = case ruleAct r of
                  ClassScore c _ -> c
                  _ -> 0
      in IM.insertWith (+) cls v acc
  | otherwise = acc
