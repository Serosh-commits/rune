module Main where

import Control.DeepSeq
import qualified Data.Vector as V
import Rune
import System.CPUTime
import Text.Printf

main :: IO ()
main = do
  start <- getCPUTime

  printf "Generating 2000 data points (3 classes)...\n"
  data_points <- spirals 2000 3
  () <- return $ rnf data_points

  printf "Initializing model...\n"
  let initial_model = sift_advanced data_points
  () <- return $ rnf initial_model

  printf "Training for 100 generations...\n"
  let final_model = train_loop_advanced 100 data_points initial_model
  () <- return $ rnf final_model

  let correct = V.length $ V.filter (\(fv, l) -> infer final_model fv == l) data_points
  let accuracy = fromIntegral correct / fromIntegral (V.length data_points) :: Double

  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10.0 ** 12.0) :: Double

  printf "Final Accuracy: %.2f%%\n" (accuracy * 100)
  printf "Training Time: %.4f s\n" diff

  save_model "rune_advanced.model" final_model
  printf "Model saved to rune_advanced.model\n"
