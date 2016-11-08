module Main where

import Language.Atom
import Language.Sally

import AtomOM1 (om1)

main :: IO ()
main = do
  putStrLn "Compiling OM1 to C... (om1.{c,h})"
  compileOM1ToC

  putStrLn "Compiling OM1 to Sally... (om1.mcmt)"
  compileToSally "om1" defaultCfg "om1.mcmt" om1 Nothing

  putStrLn "Done."


-- C Code Generator ------------------------------------------------------

-- | Invoke the atom compiler, generating 'om1.{c,h}'
-- Also print out info on the generated schedule.
compileOM1ToC :: IO ()
compileOM1ToC = do
  res <- compile "om1" cfg om1
  putStrLn $ reportSchedule (compSchedule res)
  where
    cfg = defaults { cCode = prePostCode }

-- | Custom pre-post code for generated C
prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ =
  ( unlines [ "#include <stdio.h>"
            , "#include <unistd.h>"
            , ""
            , "// ---- BEGIN of source automatically generated by Atom ----"
            ]
  , unlines [ "// ---- END of source automatically generated by Atom ----"
            , ""
            , "int main(int argc, char **argv) {"
            , "  while(1) { om1(); usleep(500); }"
            , "}"
            ]
  )
