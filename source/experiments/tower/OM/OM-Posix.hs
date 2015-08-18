module Main where

import Ivory.OS.Posix.Tower
import OM

main :: IO ()
main =
  compileTowerPosix (const $ return ()) system

