# Using Rune

Rune is designed to be simple. You don't need a massive config file or a GPU to run it. Here's how to actually get things moving.

### Prerequisites
You just need `GHC` and `cabal`. If you don't have them, use [GHCup](https://www.haskell.org/ghcup/). It's the standard way to install Haskell these days.

### The Demo
If you just want to see it work, there's a demo built in that tries to classify a spiral dataset.
```bash
make run
```
This will run the training loop for 100 generations and save the resulting model as `rune_advanced.model`. It's a good way to verify everything is compiled correctly.

### Using it in your code
The core workflow is:
1. Wrap your features in a `FeatureVector`. 
2. Call `sift_advanced` to find initial rules.
3. Call `train_loop_advanced` to let them evolve.

Here's a minimal example:

```haskell
import Rune
import qualified Data.Vector as V
import GHC.Exts (unsafeCoerce)

-- Features are stored in a boxed Vector Any. 
-- Just use unsafeCoerce to put Doubles (numeric) or Ints (categorical) in there.
let fv = FeatureVector (V.fromList [unsafeCoerce 1.2, unsafeCoerce 3.4]) mempty
let trainData = V.fromList [(fv, 0)] -- (FeatureVector, Label)

-- Bootstrapping the model
let seed = sift_advanced trainData
let model = train_loop_advanced 100 trainData seed

-- Predicting
let result = infer model fv
```

### Saving and Loading
The serialization is custom and should be faster than standard generic approaches.
```haskell
save_model "my.model" model
m <- load_model "my.model"
```

If the induction logic seems too slow or too fast, you can poke around in `sift_advanced` to adjust the thresholds, but 100 generations is usually a decent starting point.
