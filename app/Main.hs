module Main where

import Rune
import qualified Data.Vector as V
import System.CPUTime
import Text.Printf
import Control.Monad (forM_)

main :: IO ()
main = do
  start <- getCPUTime
  
  -- Larger, harder dataset
  printf "Generating 2000 data points (3 classes)...\n"
  data_points <- spirals 2000 3
  
  printf "Initializing advanced evolutionary model...\n"
  let initial_model = sift_advanced data_points
  
  printf "Training for 100 generations with evolutionary boosting...\n"
  let final_model = train_loop_advanced 100 data_points initial_model
  
  let correct = V.length $ V.filter (\(fv, l) -> infer final_model fv == l) data_points
  let accuracy = fromIntegral correct / fromIntegral (V.length data_points) :: Double
  
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12) :: Double
  
  printf "Final Accuracy: %.2f%%\n" (accuracy * 100)
  printf "Training Time: %.4f ms\n" (diff * 1000)
  
  save_model "rune_advanced.model" final_model
  printf "Model saved to rune_advanced.model\n"
