module Main (main) where

import           Control.Monad
import           GHC
import           GHC.Paths     (libdir)


-- TODO http://www.stephendiehl.com/posts/ghc_01.html
main :: IO ()
main = void $ example

example :: IO SuccessFlag
example = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              }

  target <- guessTarget "tests/Driver.hs" Nothing
  addTarget target
  load LoadAllTargets


