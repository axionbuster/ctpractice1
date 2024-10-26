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
it says 10
-}
experiment1' :: ContT Int IO Int
experiment1' = resetT do
  -- discarding the continuation causes early termination
  point "1"
  z <- shiftT \(_k :: Int -> IO Int) -> do
    point "2"
    point "3" -- no "v" value to speak of, since we don't call _k
    pure 10 -- ends here ...

  -- [unreachable code below]
  -- [this part, captured as a continuation, will never be reached,
  -- having been discarded by the "shift" call]
  point $ "4: " ++ show z
  pure (z - 1) -- ... instead of here

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
      -- closest "reset" call
      point "2"
      b <- shiftT \k -> do
        point "3"
        c <- lift $ k 100
        point $ "4: " ++ show c
        pure (c * 2)
      point $ "5: " ++ show b
      -- after this pure action, we jump to where we left off
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
  -- Putting the "delimited" in "delimited continuations"
  resetT do
    -- closest "reset" call, now
    point "1"
    a {- resetT -} <- do
      point "2"
      b <- shiftT \k -> do
        point "3"
        c <- lift $ k 100
        point $ "4: " ++ show c
        pure (c * 2)
      point $ "5: " ++ show b
      pure (b * 3)
    point $ "6: " ++ show a
    -- after this pure action, we jump to where we left off
    pure (a * 5)

-- Adapted and simplified from one of Kiselyov's examples.

infixl 4 +!

(+!) :: (Monad m, Num a) => m a -> m a -> m a
a +! b = do
  x <- a
  y <- b
  pure (x + y)

-- Note: no need for resetT here, but no change if we add it.

-- 3
experiment3 :: ContT Int IO Int
experiment3 = do pure 1 +! shiftT \_ -> pure 3 -- discarded continuation

-- 4
experiment3' :: ContT Int IO Int
experiment3' = do pure 1 +! shiftT \k -> lift $ k 3 -- used continuation

-- 4 (no CPS)
experiment3'' :: ContT Int IO Int
experiment3'' = pure 1 +! pure 3 -- no CPS, no continuation

runExperiment :: (Show a) => ContT a IO a -> IO ()
runExperiment experiment =
  runContT experiment pure >>= putStrLn . ("it says " ++) . show

main :: IO ()
main = do
  putStrLn "experiment1"
  runExperiment experiment1
  putStrLn "experiment1'"
  runExperiment experiment1'
  putStrLn "experiment2"
  runExperiment experiment2
  putStrLn "experiment2'"
  runExperiment experiment2'
  putStrLn "experiment3"
  runExperiment experiment3
  putStrLn "experiment3'"
  runExperiment experiment3'
  putStrLn "experiment3''"
  runExperiment experiment3''

{-
Side note. Implementation of shift/reset.

If types didn't matter, shift/reset could be implemented as:

reset = run             -- (run :: ContT a m a -> m a)
shift f = run . f

where run k = runContT k pure.

In the implementation given by 'transformers', 'run' is called 'evalContT'
instead, defined identically.

So it's safe to use 'shift' without a 'reset' since a call to 'run' is
already equivalent to a 'reset'.

But types do matter, and so we have to write:

reset = lift . reset -- put the monad back, which means to construct newtype
shift f = ContT (run . f) -- construct newtype

But it might be helpful to note that newtype information is fully erased
at runtime, which means that the primitives above can be implemented by
composing closures, and no fancy machinery is needed.

-}
