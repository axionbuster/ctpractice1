#!/usr/bin/env stack
{-# LANGUAGE BlockArguments #-}

{- stack script
   --resolver lts-22.39
   --package transformers
   --package base
-}

-- | Demonstrate shift/reset.
--
-- The ContT implementation in transformers uses delimited continuations,
-- but the one in mtl uses the old-fashioned kind of continuations (call/cc).
--
-- Try not to mix them up:
--
-- * transformers: 'Control.Monad.Trans.Cont'
-- * mtl: 'Control.Monad.Cont'
module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO)) -- base
import Control.Monad.Trans.Class (MonadTrans (lift)) -- transformers
import Control.Monad.Trans.Cont (ContT (runContT), resetT, shiftT) -- (ditto)
import Debug.Trace (traceIO) -- base

point :: String -> ContT a IO ()
point = liftIO . traceIO

{-
1
2
4: 10
3: 9
it says 10
-}
experiment1 :: ContT Int IO Int
experiment1 = resetT do
  point "1"
  z <- shiftT \(k :: Int -> IO Int) -> do
    point "2"
    v <- lift $ k 10
    point $ "3: " ++ show v
    pure (v + 1)
  point $ "4: " ++ show z
  pure (z - 1)

{-
1
2
3
5: 100
4: 300
6: 600
it says 3000
-}
experiment2 :: ContT Int IO Int
experiment2 =
  resetT do
    point "1"
    a <- resetT do
      point "2"
      b <- shiftT \k -> do
        point "3"
        c <- lift $ k 100
        point $ "4: " ++ show c
        pure (c * 2)
      point $ "5: " ++ show b
      pure (b * 3)
    point $ "6: " ++ show a
    pure (a * 5)

{-
1
2
3
5: 100
6: 300
4: 1500
it says 3000
-}
experiment2' :: ContT Int IO Int
experiment2' =
  resetT do
    point "1"
    a <- do
      point "2"
      b <- shiftT \k -> do
        point "3"
        c <- lift $ k 100
        point $ "4: " ++ show c
        pure (c * 2)
      point $ "5: " ++ show b
      pure (b * 3)
    point $ "6: " ++ show a
    pure (a * 5)

runExperiment :: (Show a) => ContT a IO a -> IO ()
runExperiment experiment =
  runContT experiment pure >>= putStrLn . ("it says " ++) . show

main :: IO ()
main = do
  putStrLn "experiment1"
  runExperiment experiment1
  putStrLn "experiment2"
  runExperiment experiment2
  putStrLn "experiment2'"
  runExperiment experiment2'