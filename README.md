# rune

A fast Haskell library for rule-based machine learning. I got tired of black-box models that you can't debug, so I built this. It uses evolutionary logic to find rules that actually make sense.

### How it works
It's basically a genetic algorithm that sifts through your data to find predicates (like `x > 5`). It then combines them using AND/OR gates and evolves them over several generations. It uses some `unsafeCoerce` magic internally to keep things fast without sacrificing too much safety at the API level.

### Quick Start
You'll need `cabal`. Just clone and run the demo:
```bash
make run
```

If you want to use it in your own code:
```haskell
import Rune

-- Sift through data to find basic candidates
let seed = sift_advanced training_data

-- Let them evolve for a while
let model = train_loop_advanced 100 training_data seed

-- Predict
let res = infer model some_fv
```

### Why use this?
- **Small**: Very few dependencies. Just the essentials like `vector` and `hashable`.
- **Fast**: Hand-rolled bit-level serialization and strict evaluation for performance.
- **Readable**: You can actually print the rules and see *why* the model made a choice.
