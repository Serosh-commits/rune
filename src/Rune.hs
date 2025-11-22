{-# LANGUAGE BangPatterns, DeriveFoldable, DeriveFunctor, DeriveTraversable, RankNTypes, ScopedTypeVariables, PatternSynonyms, ViewPatterns, GADTs, DataKinds, KindSignatures, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Rune where

import Prelude hiding (lookup, filter, foldl, foldr, map, pred)
import qualified Prelude as P
import Control.Monad (foldM, forM_, when, guard, replicateM, join)
import Control.Monad.ST (ST, runST)
import Control.DeepSeq (NFData(..), deepseq)
import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortBy, maximumBy)
import Data.Ord (comparing)
import Data.Primitive.ByteArray (ByteArray)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as VA
import GHC.Exts (Any, unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word64, Word8, Word32)
import Data.Int (Int64)

newtype Score = Score { un_score :: Double }
  deriving (Show, Eq, Ord)

data FeatureType = FNum | FCat
  deriving (Show, Eq, Enum)

instance NFData FeatureType where
  rnf !x = ()

data Metadata = Metadata
  { meta_type :: !FeatureType
  , meta_name :: !ByteString
  } deriving (Show, Eq)

instance NFData Metadata where
  rnf (Metadata t n) = rnf t `seq` rnf n

data FeatureVector = FeatureVector
  { fv_data :: !(Vector Any)
  , fv_meta :: !(IntMap Metadata)
  }

instance Show FeatureVector where
  show _ = "<feature_vector>"

instance NFData FeatureVector where
  rnf (FeatureVector d m) = d `seq` rnf m

get_num :: FeatureVector -> Int -> Double
get_num fv i = unsafeCoerce (V.unsafeIndex (fv_data fv) i)
{-# INLINE get_num #-}

get_cat :: FeatureVector -> Int -> Int
get_cat fv i = unsafeCoerce (V.unsafeIndex (fv_data fv) i)
{-# INLINE get_cat #-}

data Predicate where
  IsEq :: !Int -> !Int -> Predicate
  IsGt :: !Int -> !Double -> Predicate
  IsLt :: !Int -> !Double -> Predicate
  InRange :: !Int -> !Double -> !Double -> Predicate
  Oblique :: !(VU.Vector Double) -> !Double -> Predicate
  And :: !Predicate -> !Predicate -> Predicate
  Or :: !Predicate -> !Predicate -> Predicate
  Not :: !Predicate -> Predicate
  TruePred :: Predicate

instance Show Predicate where
  show (IsEq i v) = "x[" ++ show i ++ "] == " ++ show v
  show (IsGt i v) = "x[" ++ show i ++ "] > " ++ show v
  show (IsLt i v) = "x[" ++ show i ++ "] < " ++ show v
  show (InRange i l h) = show l ++ " < x[" ++ show i ++ "] < " ++ show h
  show (Oblique _ _) = "oblique"
  show (And a b) = "(" ++ show a ++ " && " ++ show b ++ ")"
  show (Or a b) = "(" ++ show a ++ " || " ++ show b ++ ")"
  show (Not a) = "!(" ++ show a ++ ")"
  show TruePred = "true"

instance NFData Predicate where
  rnf (IsEq i v) = rnf i `seq` rnf v
  rnf (IsGt i v) = rnf i `seq` rnf v
  rnf (IsLt i v) = rnf i `seq` rnf v
  rnf (InRange i l h) = rnf i `seq` rnf l `seq` rnf h
  rnf (Oblique v t) = rnf v `seq` rnf t
  rnf (And a b) = rnf a `seq` rnf b
  rnf (Or a b) = rnf a `seq` rnf b
  rnf (Not a) = rnf a
  rnf TruePred = ()

data ActionRep
  = ClassScore !Int !Double
  | Regress !Double
  deriving (Show, Eq)

instance NFData ActionRep where
  rnf (ClassScore c s) = rnf c `seq` rnf s
  rnf (Regress v) = rnf v

type Action = FeatureVector -> Score

eval_action :: ActionRep -> Action
eval_action (ClassScore _ s) _ = Score s
eval_action (Regress v) _ = Score v
{-# INLINE eval_action #-}

data Rule = Rule
  { r_fingerprint :: !Word64
  , r_predicate :: !Predicate
  , r_action_rep :: !ActionRep
  , r_weight :: !Double
  , r_coverage :: !Int
  , r_correct :: !Int
  , r_generation :: !Int
  } deriving (Show)

instance NFData Rule where
  rnf (Rule f p a w c r g) = rnf f `seq` rnf p `seq` rnf a `seq` rnf w `seq` rnf c `seq` rnf r `seq` rnf g

newtype RuleSet = RuleSet
  { rs_rules :: HM.HashMap Word64 Rule
  } deriving (Show)

instance NFData RuleSet where
  rnf (RuleSet r) = rnf r

hash_pred :: Predicate -> Word64
hash_pred (IsEq i v) = hash64 i `xor` hash64 v `xor` 0x12345678
hash_pred (IsGt i v) = hash64 i `xor` hash64 v `xor` 0x87654321
hash_pred (IsLt i v) = hash64 i `xor` hash64 v `xor` 0xDEADBEEF
hash_pred (InRange i l h) = hash64 i `xor` hash64 l `xor` hash64 h `xor` 0xCAFEBABE
hash_pred (Oblique v t) = hash64 (VU.toList v) `xor` hash64 t
hash_pred (And a b) = hash_pred a `xor` rotate (hash_pred b) 13
hash_pred (Or a b) = hash_pred a `xor` rotate (hash_pred b) 19
hash_pred (Not a) = hash_pred a `xor` 0xFFFFFFFFFFFFFFFF
hash_pred TruePred = 0

hash64 :: Hashable a => a -> Word64
hash64 = fromIntegral . hash
{-# INLINE hash64 #-}

rotate :: Word64 -> Int -> Word64
rotate x k = (x `shiftL` k) .|. (x `shiftR` (64 - k))
{-# INLINE rotate #-}

matches :: Predicate -> FeatureVector -> Bool
matches (IsEq i v) fv = get_cat fv i == v
matches (IsGt i v) fv = get_num fv i > v
matches (IsLt i v) fv = get_num fv i < v
matches (InRange i l h) fv = let v = get_num fv i in v > l && v < h
matches (Oblique coeffs t) fv =
  let dot = VU.ifoldl' (\acc idx c -> acc + c * get_num fv idx) 0.0 coeffs
  in dot > t
matches (And a b) fv = matches a fv && matches b fv
matches (Or a b) fv = matches a fv || matches b fv
matches (Not a) fv = not (matches a fv)
matches TruePred _ = True
{-# INLINE matches #-}

infer :: RuleSet -> FeatureVector -> Int
infer rs fv =
  let rules = HM.elems (rs_rules rs)
      scores = foldl' (tally fv) IM.empty rules
  in if IM.null scores then 0 else fst $ maximumBy (comparing snd) (IM.toList scores)

tally :: FeatureVector -> IntMap Double -> Rule -> IntMap Double
tally fv acc rule =
  if matches (r_predicate rule) fv
  then
    let (Score s) = eval_action (r_action_rep rule) fv
        w = r_weight rule
        conf = (fromIntegral (r_correct rule) + 1.0) / (fromIntegral (r_coverage rule) + 2.0)
        final = s * w * conf
        cls = case r_action_rep rule of
                ClassScore c _ -> c
                _ -> 0
    in IM.insertWith (+) cls final acc
  else acc
{-# INLINE tally #-}

forge_rule :: Predicate -> ActionRep -> Rule
forge_rule p a = Rule
  { r_fingerprint = hash_pred p
  , r_predicate = p
  , r_action_rep = a
  , r_weight = 1.0
  , r_coverage = 0
  , r_correct = 0
  , r_generation = 0
  }

add_rule :: Rule -> RuleSet -> RuleSet
add_rule r rs = rs { rs_rules = HM.insert (r_fingerprint r) r (rs_rules rs) }

remove_rule :: Word64 -> RuleSet -> RuleSet
remove_rule fp rs = rs { rs_rules = HM.delete fp (rs_rules rs) }

boost_rule :: Word64 -> Double -> RuleSet -> RuleSet
boost_rule fp alpha rs =
  rs { rs_rules = HM.adjust (\r -> r { r_weight = r_weight r * exp alpha }) fp (rs_rules rs) }

anneal :: RuleSet -> FeatureVector -> Int -> RuleSet
anneal rs fv label =
  rs { rs_rules = HM.map (\r ->
         if matches (r_predicate r) fv
         then
           let is_correct = case r_action_rep r of
                              ClassScore c _ -> c == label
                              _ -> False
           in r { r_coverage = r_coverage r + 1
                , r_correct = r_correct r + (if is_correct then 1 else 0)
                }
         else r
       ) (rs_rules rs) }

spirals :: Int -> Int -> IO (Vector (FeatureVector, Int))
spirals n classes = do
  return $ V.generate n $ \i ->
    let c = i `mod` classes
        t = (fromIntegral i / fromIntegral n) * 4.0 * pi + (fromIntegral c * 2.0 * pi / fromIntegral classes)
        r = (fromIntegral i / fromIntegral n) * 5.0 + 0.2
        noise = (fromIntegral (hash64 i `mod` 100) / 100.0) * 0.2
        x = r * cos t + noise
        y = r * sin t + noise
        fv = FeatureVector (V.fromList [unsafeCoerce x, unsafeCoerce y]) IM.empty
    in (fv, c)

class Serial a where
  serialize :: a -> BB.Builder
  deserialize :: ByteString -> (a, ByteString)

instance Serial Int where
  serialize = BB.int64LE . fromIntegral
  deserialize bs = 
    let (h, t) = B.splitAt 8 bs
        val = foldr (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0 (B.unpack (B.reverse h))
    in (fromIntegral val, t)

instance Serial Double where
  serialize = BB.doubleLE
  deserialize bs = 
    let (h, t) = B.splitAt 8 bs
        w64 = foldr (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0 (B.unpack (B.reverse h)) :: Word64
    in (unsafeCoerce w64, t)

instance Serial Word64 where
  serialize = BB.word64LE
  deserialize bs =
    let (h, t) = B.splitAt 8 bs
        val = foldr (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0 (B.unpack (B.reverse h))
    in (val, t)

instance Serial Predicate where
  serialize (IsEq i v) = BB.word8 0 <> serialize i <> serialize v
  serialize (IsGt i v) = BB.word8 1 <> serialize i <> serialize v
  serialize (IsLt i v) = BB.word8 2 <> serialize i <> serialize v
  serialize (InRange i l h) = BB.word8 3 <> serialize i <> serialize l <> serialize h
  serialize (Oblique v t) = BB.word8 4 <> serialize (VU.length v) <> foldMap serialize (VU.toList v) <> serialize t
  serialize (And a b) = BB.word8 5 <> serialize a <> serialize b
  serialize (Or a b) = BB.word8 6 <> serialize a <> serialize b
  serialize (Not a) = BB.word8 7 <> serialize a
  serialize TruePred = BB.word8 8
  deserialize bs =
    let (tag, rest) = B.splitAt 1 bs
        t = if B.null tag then 0 else B.head tag
    in case t of
      0 -> let (i, r1) = deserialize rest; (v, r2) = deserialize r1 in (IsEq i v, r2)
      1 -> let (i, r1) = deserialize rest; (v, r2) = deserialize r1 in (IsGt i v, r2)
      2 -> let (i, r1) = deserialize rest; (v, r2) = deserialize r1 in (IsLt i v, r2)
      3 -> let (i, r1) = deserialize rest; (l, r2) = deserialize r1; (h, r3) = deserialize r2 in (InRange i l h, r3)
      4 -> let (len, r1) = deserialize rest
               (vals, r2) = des_list len r1 []
               (thresh, r3) = deserialize r2
           in (Oblique (VU.fromList vals) thresh, r3)
      5 -> let (a, r1) = deserialize rest; (b, r2) = deserialize r1 in (And a b, r2)
      6 -> let (a, r1) = deserialize rest; (b, r2) = deserialize r1 in (Or a b, r2)
      7 -> let (a, r1) = deserialize rest in (Not a, r1)
      8 -> (TruePred, rest)
      _ -> error "bad pred tag"

des_list :: Serial a => Int -> ByteString -> [a] -> ([a], ByteString)
des_list 0 bs acc = (reverse acc, bs)
des_list n bs acc = let (x, rest) = deserialize bs in des_list (n-1) rest (x:acc)

instance Serial ActionRep where
  serialize (ClassScore c s) = BB.word8 0 <> serialize c <> serialize s
  serialize (Regress v) = BB.word8 1 <> serialize v
  deserialize bs =
    let (tag, rest) = B.splitAt 1 bs
        t = if B.null tag then 0 else B.head tag
    in case t of
      0 -> let (c, r1) = deserialize rest; (s, r2) = deserialize r1 in (ClassScore c s, r2)
      1 -> let (v, r1) = deserialize rest in (Regress v, r1)
      _ -> error "bad action tag"

instance Serial Rule where
  serialize r = serialize (r_fingerprint r) <> serialize (r_predicate r) <> serialize (r_action_rep r) <> serialize (r_weight r) <> serialize (r_coverage r) <> serialize (r_correct r) <> serialize (r_generation r)
  deserialize bs =
    let (f, r1) = deserialize bs
        (p, r2) = deserialize r1
        (a, r3) = deserialize r2
        (w, r4) = deserialize r3
        (c, r5) = deserialize r4
        (cor, r6) = deserialize r5
        (g, r7) = deserialize r6
    in (Rule f p a w c cor g, r7)

instance Serial RuleSet where
  serialize rs =
    let rules = HM.elems (rs_rules rs)
    in serialize (length rules) <> foldMap serialize rules
  deserialize bs =
    let (len, r1) = deserialize bs
        (rules, r2) = des_list len r1 []
    in (RuleSet $ HM.fromList [ (r_fingerprint r, r) | r <- rules ], r2)

save_model :: FilePath -> RuleSet -> IO ()
save_model path rs = do
  let b = BB.toLazyByteString (BB.stringUtf8 "RUNE02" <> serialize rs)
  BL.writeFile path b

load_model :: FilePath -> IO RuleSet
load_model path = do
  bs <- B.readFile path
  let (ver, rest) = B.splitAt 6 bs
  if ver /= B.pack [82,85,78,69,48,50]
    then error "bad version"
    else return $ fst $ deserialize rest

sift_advanced :: Vector (FeatureVector, Int) -> RuleSet
sift_advanced data_points =
  let candidates = [0, 1]
      thresholds = [-5.0, -4.5 .. 5.0]
      base_rules = do
        dim <- candidates
        t <- thresholds
        op <- [IsGt, IsLt]
        let pred = op dim t
        let (l_true, l_false) = V.partition (\(fv, _) -> matches pred fv) data_points
        guard (not (V.null l_true))
        let counts = foldl' (\acc (_, c) -> IM.insertWith (+) c 1 acc) IM.empty l_true
        let (best_c, count) = maximumBy (comparing snd) (IM.toList counts)
        let accuracy = fromIntegral count / fromIntegral (V.length l_true)
        guard (accuracy > 0.55)
        return $ forge_rule pred (ClassScore best_c accuracy)
      
      complex_rules = do
        r1 <- take 20 base_rules
        r2 <- take 20 (drop 20 base_rules)
        op <- [And, Or]
        let pred = op (r_predicate r1) (r_predicate r2)
        let (l_true, l_false) = V.partition (\(fv, _) -> matches pred fv) data_points
        guard (not (V.null l_true))
        let counts = foldl' (\acc (_, c) -> IM.insertWith (+) c 1 acc) IM.empty l_true
        let (best_c, count) = maximumBy (comparing snd) (IM.toList counts)
        let accuracy = fromIntegral count / fromIntegral (V.length l_true)
        guard (accuracy > 0.65)
        return $ forge_rule pred (ClassScore best_c accuracy)

  in RuleSet $ HM.fromList [ (r_fingerprint r, r) | r <- base_rules ++ complex_rules ]

evolve :: RuleSet -> Vector (FeatureVector, Int) -> RuleSet
evolve rs data_points =
  let rules = HM.elems (rs_rules rs)
      best_rules = take 50 $ sortBy (flip (comparing r_correct)) rules
      new_rules = do
        r1 <- best_rules
        r2 <- best_rules
        guard (r_fingerprint r1 /= r_fingerprint r2)
        let p1 = r_predicate r1
        let p2 = r_predicate r2
        op <- [And, Or]
        let new_pred = op p1 p2
        return $ forge_rule new_pred (r_action_rep r1)
      
      validated_rules = filter (\r -> 
          let (l_true, _) = V.partition (\(fv, _) -> matches (r_predicate r) fv) data_points
          in not (V.null l_true)
        ) new_rules
      
  in foldl' (flip add_rule) rs validated_rules

train_loop_advanced :: Int -> Vector (FeatureVector, Int) -> RuleSet -> RuleSet
train_loop_advanced 0 _ rs = rs
train_loop_advanced n data_points rs =
  let rs_annealed = V.foldl' (\acc (fv, l) -> anneal acc fv l) rs data_points
      rs_evolved = if n `mod` 10 == 0 then evolve rs_annealed data_points else rs_annealed
  in train_loop_advanced (n - 1) data_points rs_evolved
