{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rune
  ( Score (..),
    FeatureType (..),
    Metadata (..),
    FeatureVector (..),
    Predicate (..),
    ActionRep (..),
    Rule (..),
    RuleSet (..),
    infer,
    sift_advanced,
    train_loop_advanced,
    save_model,
    load_model,
    spirals,
    matches,
  )
where

import Control.DeepSeq
import Control.Monad (guard)
import Data.Bits (shiftL, shiftR, xor, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup)

newtype Score = Score {unScore :: Double}
  deriving (Show, Eq, Ord)

data FeatureType = FNum | FCat
  deriving (Show, Eq, Enum)

instance NFData FeatureType where
  rnf !_ = ()

data Metadata = Metadata
  { metaType :: !FeatureType,
    metaName :: !ByteString
  }
  deriving (Show, Eq)

instance NFData Metadata where
  rnf (Metadata t n) = rnf t `seq` rnf n

data FeatureVector = FeatureVector
  { features :: !(Vector Any),
    metadata :: !(IntMap Metadata)
  }

instance Show FeatureVector where
  show _ = "<fv>"

instance NFData FeatureVector where
  rnf (FeatureVector d m) = d `seq` rnf m

getNum :: FeatureVector -> Int -> Double
getNum fv i = unsafeCoerce (V.unsafeIndex (features fv) i)
{-# INLINE getNum #-}

getCat :: FeatureVector -> Int -> Int
getCat fv i = unsafeCoerce (V.unsafeIndex (features fv) i)
{-# INLINE getCat #-}

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

data Rule = Rule
  { fingerprint :: !Word64,
    predicate :: !Predicate,
    action :: !ActionRep,
    weight :: !Double,
    coverage :: !Int,
    correct :: !Int,
    generation :: !Int
  }
  deriving (Show)

instance NFData Rule where
  rnf (Rule f p a w c r g) =
    rnf f `seq`
      rnf p `seq`
        rnf a `seq`
          rnf w `seq`
            rnf c `seq`
              rnf r `seq`
                rnf g

newtype RuleSet = RuleSet
  { rules :: HM.HashMap Word64 Rule
  }
  deriving (Show)

instance NFData RuleSet where
  rnf (RuleSet r) = rnf r

hash64 :: (Hashable a) => a -> Word64
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
hashPred (Or a b) = hashPred a `xor` rotate64 (hashPred b) 19
hashPred (Not a) = hashPred a `xor` maxBound
hashPred TruePred = 0

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
matches (Or a b) fv = matches a fv || matches b fv
matches (Not a) fv = not (matches a fv)
matches TruePred _ = True

infer :: RuleSet -> FeatureVector -> Int
infer (RuleSet rs) fv =
  let acc = foldl' (tally fv) IM.empty (HM.elems rs)
   in if IM.null acc then 0 else fst (maximumBy (comparing snd) (IM.toList acc))

tally :: FeatureVector -> IntMap Double -> Rule -> IntMap Double
tally fv acc r
  | matches (predicate r) fv =
      let s = case action r of
            ClassScore _ score -> score
            Regress v -> v
          conf =
            (fromIntegral (correct r) + 1.0)
              / (fromIntegral (coverage r) + 2.0 :: Double)
          v = s * weight r * conf
          cls = case action r of
            ClassScore c _ -> c
            _ -> 0
       in IM.insertWith (+) cls v acc
  | otherwise = acc

forgeRule :: Predicate -> ActionRep -> Rule
forgeRule p a =
  Rule
    { fingerprint = hashPred p,
      predicate = p,
      action = a,
      weight = 1.0,
      coverage = 0,
      correct = 0,
      generation = 0
    }

addRule :: Rule -> RuleSet -> RuleSet
addRule r (RuleSet rs) = RuleSet $ HM.insert (fingerprint r) r rs

batchAnneal :: RuleSet -> Vector (FeatureVector, Int) -> RuleSet
batchAnneal (RuleSet rs) points =
  RuleSet $ HM.map (\r -> V.foldl' (update r) r points) rs
  where
    update r acc (fv, label)
      | matches (predicate r) fv =
          let isCorrect = case action r of
                ClassScore c _ -> c == label
                _ -> False
           in acc
                { coverage = coverage acc + 1,
                  correct = correct acc + (if isCorrect then 1 else 0)
                }
      | otherwise = acc

sift_advanced :: Vector (FeatureVector, Int) -> RuleSet
sift_advanced points
  | V.null points = RuleSet HM.empty
  | otherwise =
      let fv0 = fst (V.head points)
          numFeatures = V.length (features fv0)
          thresholds = [-5.0, -4.5 .. 5.0]
          baseRules = do
            dim <- [0 .. numFeatures - 1]
            t <- thresholds
            op <- [IsGt, IsLt]
            let p = op dim t
            let lTrue = V.filter (\(fv, _) -> matches p fv) points
            guard (not (V.null lTrue))
            let counts = foldl' (\acc (_, c) -> IM.insertWith (+) c (1 :: Int) acc) (IM.empty :: IntMap Int) lTrue
            let (bestC, count) = maximumBy (comparing snd) (IM.toList counts)
            let accRatio = fromIntegral count / fromIntegral (V.length lTrue)
            guard (accRatio > 0.52)
            return $ forgeRule p (ClassScore bestC accRatio)
          obliqueRules = do
            i <- [0 .. 10 :: Int]
            let w0 = fromIntegral (hash64 i `mod` 200 - 100) / 100.0
            let w1 = fromIntegral (hash64 (i + 13) `mod` 200 - 100) / 100.0
            let t = fromIntegral (hash64 (i + 42) `mod` 1000 - 500) / 100.0
            let p = Oblique (VU.fromList [w0, w1]) t
            let lTrue = V.filter (\(fv, _) -> matches p fv) points
            guard (not (V.null lTrue))
            let counts = foldl' (\acc (_, c) -> IM.insertWith (+) c (1 :: Int) acc) (IM.empty :: IntMap Int) lTrue
            let (bestC, count) = maximumBy (comparing snd) (IM.toList counts)
            let accRatio = fromIntegral count / fromIntegral (V.length lTrue)
            guard (accRatio > 0.55)
            return $ forgeRule p (ClassScore bestC accRatio)
          complexRules = do
            r1 <- take 20 baseRules
            r2 <- take 20 (drop 5 obliqueRules ++ drop 5 baseRules)
            guard (fingerprint r1 /= fingerprint r2)
            op <- [And, Or]
            let p = op (predicate r1) (predicate r2)
            let lTrue = V.filter (\(fv, _) -> matches p fv) points
            guard (not (V.null lTrue))
            let counts = foldl' (\acc (_, c) -> IM.insertWith (+) c (1 :: Int) acc) (IM.empty :: IntMap Int) lTrue
            let (bestC, count) = maximumBy (comparing snd) (IM.toList counts)
            let accRatio = fromIntegral count / fromIntegral (V.length lTrue)
            guard (accRatio > 0.60)
            return $ forgeRule p (ClassScore bestC accRatio)
       in RuleSet $ HM.fromList [(fingerprint r, r) | r <- baseRules ++ obliqueRules ++ complexRules]

evolve :: RuleSet -> Vector (FeatureVector, Int) -> RuleSet
evolve (RuleSet rs) points =
  let currentRules = HM.elems rs
      bestRules = take 50 $ sortBy (flip (comparing correct)) currentRules
      newRules = do
        r1 <- bestRules
        r2 <- bestRules
        guard (fingerprint r1 /= fingerprint r2)
        op <- [And, Or]
        let p = op (predicate r1) (predicate r2)
        return $ forgeRule p (action r1)
      validated =
        filter
          ( \r ->
              V.any (\(fv, _) -> matches (predicate r) fv) points
          )
          newRules
   in foldl' (flip addRule) (RuleSet rs) validated

train_loop_advanced :: Int -> Vector (FeatureVector, Int) -> RuleSet -> RuleSet
train_loop_advanced 0 _ rs = rs
train_loop_advanced n points rs =
  let rs' = batchAnneal rs points
      rs'' = if n `mod` 20 == 0 then evolve rs' points else rs'
   in train_loop_advanced (n - 1) points rs''

class Serial a where
  serialize :: a -> BB.Builder
  deserialize :: ByteString -> (a, ByteString)

instance Serial Int where
  serialize = BB.int64LE . fromIntegral
  deserialize bs =
    let (h, t) = B.splitAt 8 bs
        val :: Word64 = B.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 (B.reverse h)
     in (fromIntegral val, t)

instance Serial Double where
  serialize = BB.doubleLE
  deserialize bs =
    let (h, t) = B.splitAt 8 bs
        w64 :: Word64 = B.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 (B.reverse h)
     in (unsafeCoerce w64, t)

instance Serial Word64 where
  serialize = BB.word64LE
  deserialize bs =
    let (h, t) = B.splitAt 8 bs
        val :: Word64 = B.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 (B.reverse h)
     in (val, t)

instance Serial Predicate where
  serialize p = case p of
    IsEq i v -> BB.word8 0 <> serialize i <> serialize v
    IsGt i v -> BB.word8 1 <> serialize i <> serialize v
    IsLt i v -> BB.word8 2 <> serialize i <> serialize v
    InRange i l h -> BB.word8 3 <> serialize i <> serialize l <> serialize h
    Oblique v t -> BB.word8 4 <> serialize (VU.length v) <> foldMap serialize (VU.toList v) <> serialize t
    And a b -> BB.word8 5 <> serialize a <> serialize b
    Or a b -> BB.word8 6 <> serialize a <> serialize b
    Not a -> BB.word8 7 <> serialize a
    TruePred -> BB.word8 8
  deserialize bs =
    let (tag, rest) = B.splitAt 1 bs
     in case B.head tag of
          0 -> let (i, r1) = deserialize rest; (v, r2) = deserialize r1 in (IsEq i v, r2)
          1 -> let (i, r1) = deserialize rest; (v, r2) = deserialize r1 in (IsGt i v, r2)
          2 -> let (i, r1) = deserialize rest; (v, r2) = deserialize r1 in (IsLt i v, r2)
          3 -> let (i, r1) = deserialize rest; (l, r2) = deserialize r1; (h, r3) = deserialize r2 in (InRange i l h, r3)
          4 ->
            let (len, r1) = deserialize rest
                (vals, r2) = deserializeList len r1
                (thresh, r3) = deserialize r2
             in (Oblique (VU.fromList vals) thresh, r3)
          5 -> let (a, r1) = deserialize rest; (b, r2) = deserialize r1 in (And a b, r2)
          6 -> let (a, r1) = deserialize rest; (b, r2) = deserialize r1 in (Or a b, r2)
          7 -> let (a, r1) = deserialize rest in (Not a, r1)
          8 -> (TruePred, rest)
          _ -> error "Malformed serialize tag"

deserializeList :: (Serial a) => Int -> ByteString -> ([a], ByteString)
deserializeList 0 bs = ([], bs)
deserializeList n bs =
  let (x, rest) = deserialize bs
      (xs, rest') = deserializeList (n - 1) rest
   in (x : xs, rest')

instance Serial ActionRep where
  serialize (ClassScore c s) = BB.word8 0 <> serialize c <> serialize s
  serialize (Regress v) = BB.word8 1 <> serialize v
  deserialize bs =
    let (tag, rest) = B.splitAt 1 bs
     in case B.head tag of
          0 -> let (c, r1) = deserialize rest; (s, r2) = deserialize r1 in (ClassScore c s, r2)
          1 -> let (v, r1) = deserialize rest in (Regress v, r1)
          _ -> error "Malformed action tag"

instance Serial Rule where
  serialize r =
    serialize (fingerprint r)
      <> serialize (predicate r)
      <> serialize (action r)
      <> serialize (weight r)
      <> serialize (coverage r)
      <> serialize (correct r)
      <> serialize (generation r)
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
    let rsList = HM.elems (rules rs)
     in serialize (length rsList) <> foldMap serialize rsList
  deserialize bs =
    let (len, r1) = deserialize bs
        (rs, r2) = deserializeList len r1
     in (RuleSet $ HM.fromList [(fingerprint r, r) | r <- rs], r2)

save_model :: FilePath -> RuleSet -> IO ()
save_model path rs = do
  let b = BB.toLazyByteString (BB.stringUtf8 "RUNE02" <> serialize rs)
  BL.writeFile path b

load_model :: FilePath -> IO RuleSet
load_model path = do
  bs <- B.readFile path
  let (ver, rest) = B.splitAt 6 bs
  if ver /= B.pack [82, 85, 78, 69, 48, 50]
    then error "Unsupported model version or corrupted file"
    else return $ fst $ deserialize rest

spirals :: Int -> Int -> IO (Vector (FeatureVector, Int))
spirals n classes = do
  return $ V.generate n $ \i ->
    let c = i `mod` classes
        t = (fromIntegral i / fromIntegral n :: Double) * 4.0 * pi + (fromIntegral c * 2.0 * pi / (fromIntegral classes :: Double))
        rn = (fromIntegral i / fromIntegral n :: Double) * 5.0 + 0.2
        noise = (fromIntegral (hash64 i `mod` 100) / 100.0) * 0.2
        x = rn * cos t + noise
        y = rn * sin t + noise
        fv = FeatureVector (V.fromList [unsafeCoerce x, unsafeCoerce y]) IM.empty
     in (fv, c)
