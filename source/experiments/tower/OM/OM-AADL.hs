module Main where

import Tower.AADL
import OM

main :: IO ()
main = do
  compileTowerAADL id p system
  where
  p _ = return defaultAADLConfig
