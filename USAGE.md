# Rune Usage Guide

## 1. Installation

Rune requires the Haskell toolchain (GHC and Cabal).
Recommended installation via [GHCup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## 2. Running the Demo

To test the advanced evolutionary model on the spiral dataset:

```bash
make run
```

Or manually:

```bash
cabal run
```

This will:
1. Generate 2000 synthetic data points.
2. Initialize the model with `sift_advanced`.
3. Train for 100 generations using `train_loop_advanced`.
4. Output accuracy and training time.
5. Save the model to `rune_advanced.model`.

## 3. Using Rune for Your Data

### Step 1: Import Rune
```haskell
import Rune
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM
import GHC.Exts (unsafeCoerce)
```

### Step 2: Prepare Data
Convert your data into `Vector (FeatureVector, Int)`.
- `FeatureVector` stores features in a boxed vector (`Vector Any`).
- Use `unsafeCoerce` to store `Double` (numeric) or `Int` (categorical) values.

```haskell
mkPoint :: Double -> Double -> Int -> (FeatureVector, Int)
mkPoint x y label =
  let fv = FeatureVector 
             (V.fromList [unsafeCoerce x, unsafeCoerce y]) 
             IM.empty -- Metadata is optional for now
  in (fv, label)

my_data = V.fromList [ mkPoint 1.0 2.0 0, mkPoint 5.0 5.0 1 ]
```

### Step 3: Train
```haskell
-- 1. Sift for initial candidate rules
initial_model = sift_advanced my_data

-- 2. Evolve and Boost
-- n = number of generations (e.g., 100)
final_model = train_loop_advanced 100 my_data initial_model
```

### Step 4: Inference
```haskell
prediction = infer final_model some_feature_vector
```

### Step 5: Persistence
```haskell
save_model "my_model.rune" final_model
loaded_model <- load_model "my_model.rune"
```
