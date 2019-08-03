module Main where

-- FIXME stylish-haskell rules that don't drive me crazy
import           Control.Monad
import           Control.Monad.IO.Class
import           DynFlags
import           GHC
import           GHC.Paths              (libdir)
import           Maybes

-- TODO http://www.stephendiehl.com/posts/ghc_01.html

main :: IO ()
main = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags $ dflags {
      hscTarget = HscInterpreted
    , ghcLink   = LinkInMemory
    }
  t <- guessTarget "exe/Main.hs" Nothing
  setTargets [t]
  _ <- load LoadAllTargets

  graph <- getModuleGraph
  mss <- filterM (isLoaded . ms_mod_name) (mgModSummaries graph)
  let m = ms_mod ms
      ms = head mss

  liftIO . putStrLn $ (show . length $ mss) ++ " modules loaded"

  mi <- getModuleInfo m
  let mod_info = fromJust mi
  let names = GHC.modInfoTopLevelScope mod_info `orElse` []

  liftIO $ putStrLn $ "seen " <> (show $ length names) <> " Names"
