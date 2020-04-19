---
title: "Hspec"
date: 2020-04-09T14:38:17+08:00
draft: false
---

## 1. Recap 
code example (This code use the `Automatic Spec Discovery`)
```
module <Module name>Spec where 
import Test.Hspec
import ModuleBeingTest.AandB

spec :: Spec 
spec = do                       -- more than one `describe`
    describe "FunctionA" $ 
        do                      -- more than one `it` 
            it "can do this with A" f1 `shouldBe` resultA1
            it "can do that with A" f2 `shouldBe` resultA2
    describe "FunctionB" $ 
        do  
            it "can do this with B" B1 `shouldBe` resultB1
            it "can do that with B" B2 `shouldBe` resultB2
```

**stack test**
 
```
<Module name>               -- Test Module name with no `Spec`
  FunctionA      -- Text after `describe`
     it can do this with A  -- Text after `it`
     it can do that with A
  FunctionB
     it can do this with B FAILED [1] -- Test FAIL!!
     it can do that with B

Failures:

  <file contains failed test>:20:63: 
  1) <Module name>  Test the test-suit, it can do this with B
       expected: <resultB1>
        but got: <B1>
...
Randomized with seed 1521981406
...
Finished in 0.0004 seconds
4 examples, 1 failure

...
    <project name>-test-suite:  exited with: ExitFailure 1
Logs printed to console
```
**Test a particular describe**

`stact test --test-arguments=--match="<Module name>.FunctionB"`  
**This will only test the second describe part `FunctionB`**

**TODO: This command doesn't work if there is any space in the describe string**



## 2. Automatic Spec Discovery in Stack Project

1. Edit `package.yaml`
    > `package.yaml` is a wrapper of `<projectname>.cabal`. This free me from dealing with the differences between `exitcode-stdio-1` and `detailed-0.9`, both are different `cabal` file types.
    > - [package.yaml](https://github.com/sol/hpack#readme)
    > - [cabal keys](https://www.haskell.org/cabal/users-guide/developing-packages.html)   

    ```
    tests: 
      source-dirs: test
      main: Spec.hs
    ```
2. create folder `test/` in project.
3. create file `Spec.hs` in `test/`
```
-- file test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```
4. creat test file `TestOneSpec.hs` or `TestTwoSpec.hs` in `test/`.
  - Assume `TestOneSpec.hs` is for `ModelOne`, etc.
  - These file names must end with `Spec.hs`.
  - Each file has to export a function `spec` of type `Spec`.
    ```
    spec :: Spec 
    spec = do 
        describe "Some functionality A" $  do
            it "can do this" f1 `shouldBe` resultA1
            it "can do that" f2 `shouldBe` resultA2
        describe "Some functionality B" $  do
            it "can do this" B1 `shouldBe` resultB1
            it "can do that" B2 `shouldBe` resultB2
    ```
**Check** [official doc](https://hspec.github.io/hspec-discover.html) for more information. 

## 3. Use QuickCheck together with Hspec

- `property` function: [Official doc](https://hspec.github.io/quickcheck.html)

# TODO
- Test option examples: [Official doc](https://hspec.github.io/options.html)
- Examples about using `before_ & after_ & around_ `
- Examples using other [`should` functions](https://hspec.github.io/expectations.html).