# rune

Pure functional evolutionary rule learning. Haskell 98+.

## Gist

Rune is a high-performance, dependency-light library for rule-based machine learning. It combines **algorithms** with **AdaBoost** to forge robust decision forests from raw feature vectors.

## Specs

- **Engine**: Evolutionary rule induction with genetic recombination (AND/OR/NOT/InRange).
- **Boosting**: Adaptive reweighting of rules based on coverage and error (AdaBoost-style).
- **Types**: Heterogeneous predicates via GADTs. Boxed vectors + `unsafeCoerce` for speed.
- **Persistence**: Custom bit-level serialization. No `binary` or `cereal` overhead.
- **Deps**: `vector`, `unordered-containers`, `hashable`. That's it.

## Usage

```haskell
import Rune


let initial = sift_advanced data_points

let model = train_loop_advanced 100 data_points initial


let prediction = infer model feature_vector
```

## Build

```bash
cabal run
# or
make run
```
