# `tasty-flaky`

This provides [`tasty`](https://hackage.haskell.org/package/tasty) integration for flaky tests, which are tests that are known to fail intermittently. [`tasty-flaky`](https://hackage.haskell.org/package/tasty-flaky) can be installed from Hackage.

## Example usage

This package provides functions, like `flakyTest`, which can attach retrying logic to *any* test.
For example, you can retry test cases from [`tasty-hunit`](https://hackage.haskell.org/package/tasty-hunit) like so:

```haskell
import Test.Tasty.HUnit ( testCase ) -- from tasty-hunit

myFlakyTest :: TestTree
myFlakyTest 
    = flakyTest (limitRetries 5 <> constantDelay 1000) 
    $ testCase "some test case" 
    $ do ... 
```

In the example above, the test will be retried up to 5 times, with a delay of 1000 microseconds between tries,
if a failure occurs.
