module Main where

import AtomSMP

main :: IO ()
main = do
  putStrLn "Compiling SMP to C... (smp.{c,h})"
  compileSMPToC
  putStrLn "Compiling SMP to Sally... (smp.mcmt)"
  compileSMPToSally ";; NONE"  -- no query
  putStrLn "Done."
