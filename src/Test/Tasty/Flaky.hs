{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave Inc.
-- License     :  BSD-3-Clause
-- Maintainer  :  Laurent René de Cotret
-- Portability :  portable
--
-- This module defines a single function, 'flakyTest', to declare a test
-- which intermittently fails. Flaky tests can be retries using retry policies
-- provided by the "Control.Retry" module (from the @retry@ package).
--
--
-- For example, you can retry test cases from @tasty-hunit@ like so:
--
-- @
-- import Test.Tasty.HUnit ( testCase ) -- from tasty-hunit
-- 
-- myFlakyTest :: TestTree
-- myFlakyTest = 'flakyTest' ('limitRetries' 5 <> 'constantDelay' 1000) $ testCase "some test case" $ do ... 
-- @
--
-- In the example above, the test will be retried up to 5 times, with a delay of 1000 microseconds between tries,
-- if a failure occurs.
--
module Test.Tasty.Flaky (
    -- * Test wrapper
    flakyTest
    , flakyTestWithRetryAction

    -- * Re-exports
    -- 
    -- | The following functions allow to construct 'RetryPolicyM IO' 
    -- from the "Control.Retry" module.
    , constantDelay
    , exponentialBackoff
    , fullJitterBackoff
    , fibonacciBackoff
    , limitRetries

    -- * Policy Transformers
    , limitRetriesByDelay
    , limitRetriesByCumulativeDelay
    , capDelay

) where

import Control.Retry hiding (RetryPolicy)
import Data.Functor ( (<&>) )
import Data.Tagged (Tagged, retag )
import Test.Tasty.Providers ( IsTest(..), Progress, Result, TestTree )
import Test.Tasty.Runners ( TestTree(..), Result(..), Progress(..), emptyProgress, resultSuccessful )
import Test.Tasty.Options ( OptionDescription, OptionSet )


-- | A test tree of type @t@, with an associated retry policy
data FlakyTest t
    = MkFlakyTest (RetryStatus -> Result -> IO RetryAction) (RetryPolicyM IO) t

-- | Modify the delay of a RetryPolicy.
-- Does not change whether or not a retry is performed.
modifyRetryPolicyDelay :: Functor m => (Int -> Int) -> RetryPolicyM m -> RetryPolicyM m
modifyRetryPolicyDelay f (RetryPolicyM p) = RetryPolicyM $ \stat -> fmap f <$> p stat


-- | Mark any test as flaky.
--
-- If this test is not successful, it will be retried according to the supplied @'RetryPolicyM' 'IO'@.
-- See "Control.Retry" for documentation on how to specify a @'RetryPolicyM' 'IO'@.
--
-- For example, you can retry test cases from @tasty-hunit@ like so:
--
-- @
-- import Test.Tasty.HUnit ( testCase ) -- from tasty-hunit
--
-- myFlakyTest :: TestTree
-- myFlakyTest = 'flakyTest' ('limitRetries' 5 <> 'constantDelay' 1000) $ testCase "some test case" $ do ...
-- @
--
flakyTest :: (RetryPolicyM IO) -> TestTree -> TestTree
flakyTest = flakyTestWithRetryAction (\_ _ -> pure ConsultPolicy)

-- | Mark any test as flaky. Like 'flakyTest', but allows for overriding retry policies
-- based on test results. Also see 'RetryAction'.
--
flakyTestWithRetryAction :: (RetryStatus -> Result -> IO RetryAction) -> (RetryPolicyM IO) -> TestTree -> TestTree
flakyTestWithRetryAction retryAction policy = \case
    (SingleTest name t)           -> SingleTest name (MkFlakyTest retryAction policy t)
    (TestGroup name subtree)      -> TestGroup name (map go subtree)
    (PlusTestOptions modOption t) -> PlusTestOptions modOption (go t)
    (WithResource spec f)         -> WithResource spec (f <&> go)
    (AskOptions f)                -> AskOptions $ \optionSet -> go (f optionSet)
    (After depType expr t)        -> After depType expr (go t)
  where
    go = flakyTestWithRetryAction retryAction policy


instance IsTest t => IsTest (FlakyTest t) where
    run :: IsTest t => OptionSet -> FlakyTest t -> (Progress -> IO ()) -> IO Result
    run opts (MkFlakyTest retryAction policy test) progressCallback = go defaultRetryStatus
        where
            -- The logic below mimics the `retry` package's Control.Retry.retrying
            -- with one major difference: we annotate the final result
            -- to report how many retries have been performed, regardless of
            -- the final result.
            go :: RetryStatus -> IO Result
            go status = do
                result <- run opts test progressCallback
                let done = pure $ annotateResult status result
                    consultPolicy policy' = do
                        rs <- applyAndDelay policy' status
                        case rs of
                            -- We are done: no more retries
                            Nothing -> done
                            -- At least one more retry
                            Just rs' -> do
                                progressCallback (annotateProgress status)
                                go $! rs'

                if resultSuccessful result
                then done
                else do
                  retry <- retryAction status result
                  case retry of
                    DontRetry -> done
                    ConsultPolicy -> consultPolicy policy
                    ConsultPolicyOverrideDelay delay ->
                      consultPolicy $ modifyRetryPolicyDelay (const delay) policy

            annotateProgress :: RetryStatus -> Progress
            annotateProgress status
                -- Recall that `rsIterNumber` starts at 0, so the first attempt is rsIterNumber + 1
                = emptyProgress{progressText=mconcat ["Attempt #", show (rsIterNumber status + 1), " failed"]}

            annotateResult :: RetryStatus -> Result -> Result
            annotateResult status result
                = result { resultDescription = resultDescription result <> annotate status }
                where
                    annotate :: RetryStatus -> String
                    annotate (RetryStatus iternum cumdelay _)
                        | iternum == 0 = ""
                        | otherwise    = mconcat [" [", show iternum, " retries, ", show cumdelay, " μs delay]"]


    testOptions :: Tagged (FlakyTest t) [OptionDescription]
    testOptions = retag (testOptions :: Tagged t [OptionDescription])
