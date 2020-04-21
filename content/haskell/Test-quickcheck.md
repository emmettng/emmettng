---
title: "Quickcheck"
date: 2020-04-08T17:12:25+08:00
draft: false
---


# . WHY Quickcheck    

- Hspec to evaluate given test cases.
  - Construct test case and choose input and predict output manually.
  ```
    spec :: Spec 
    spec = do 
        describe "Some functionality A" $ 
            it "can do this" (1+1) `shouldBe` 2
  ```
  > This is the test case of function `(+)`.

- Quichcheck care about `properties`
  - Function being tested should satisfy certain property on a set of test cases. 
  - Test cases are auto generated.
  ```
    prop_RevRev xs = reverse (reverse xs) == xs
      where types = xs::[Int]
  ```
  > function `reverse` should satisfy this property. It is that reversing a list twice equivalent to doing nothing to this list at all.
  
  Quickcheck provide one possibility to test over as many use cases of a function as possible. It could offer: 
    1. Auto generated input.
    1. Auxiliary utilities to help describe the set of test cases. 


# . Use Quickcheck in stack project :
**package.yaml**
```
tests:
  discordia-test-suite:
    source-dirs: test-suite
    main: Spec.hs
    ghc-options:
    - -rtsopts
    - -threaded
    - -with-rtsopts=-N
    dependencies:
    - base
    - hspec
    - QuickCheck
    - <other useful libraries>
```

**project structure**
```
<Project folder>
  - src
  - test-suite
      - Spec.hs
      - TestModelOne
          - TestOneSpec.hs
      - TestModelTwo
          - TestTwoSpec.hs
```

- package.yaml --> test: --> source-dirs: --> test-suite
- project: In folder `test-suite`
  - `Spec.hs`
    `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}`
  - `TestModelOne contains tests for module one`
  - `TestModelTwo contains tests for module two`
    - file name must end with `Spec.hs`
  - In each `*Spec.hs` file 
    ```
    import Test.QuickCheck
    import Test.Hspec
    ...
    spec :: Spec
    spec = do
      describe "some description" $
        do it "  " $....
           it "   " $...
    .....
    ```
    `spec` must be type `Spec`


# . Recap
```
import Test.QuickCheck 

prop_rev :: [a] -> Bool
prop_rev xs = reverse (reverse xs) == xs

main = quickCheck prop_RevRev
```
- The target function is `reverse`.   
- Property: apply this function to a list `xs` twice `reverse (reverse xs)` will return the original list `xs`.  
- This property is a function
  - `prop_rev xs = reverse (reverse xs) == xs`  
  - `prop_rev :: [a] -> Bool` 
- QuickCheck generates random input(s) of prop_rev.
- Feed random input to this property function. 

# . Examples
**Norm.hs**
```
module Norm where 

import qualified Data.Vector.Storable as DV
import Numeric.LinearAlgebra hiding (Vector)

-- | Mean Absolute Difference
type Vector = DV.Vector Double 

mad' :: Vector -> Vector -> Double
mad' v1 v2 = (norm_1 v1 v2) / n                     
  where 
    n = fromIntegral . DV.length $ v1 
```
- `mad` compute mean of absolute difference
- `mad` relies on `norm_1` from `Numeric,LinearAlgebra`
- `mad` relies on `Vector` from `Data.Vector.Storable`

**TestModule.hs**
```
module TestModel where 

import Norm 
import qualified Data.Vector.Storable as DV
import Test.QuickCheck 
import Test.Hspec 

spec :: Spec
    spec = do
      describe "Norm has properties:" $  do 
           it "function mad norm1`div`vector length" $ property $ prop'mad

-- | This auxiliary function generates two list of the same length
gen'equal'length'list :: Int -> Gen ([Double], [Double])
gen'equal'length'list len =
  let v1 = sequence ([ arbitrary | _ <- [1 .. len] ] :: [Gen Double])
      v2 = sequence ([ arbitrary | _ <- [1 .. len] ] :: [Gen Double])
  in ((,)) <$> v1 <*> v2

-- | This is the version one property of mad
prop'mad :: Int -> Property
prop'mad len = forAll (gen'equal'length'list len) $ prop'
  where
    prop' vt =
      let v1 = fst vt
          v2 = snd vt
          l = fromIntegral . length $ v1
          diff =
            (mad (DV.fromList v1) (DV.fromList v2)) - (sum $ abs <$> zipWith (-) v1 v2) / l)
      in abs diff < 0.00001
```
- The input of prop'mad is `len`, the length of random lists.
- `QuickCheck` generate `len` randomly. Thus, we get two lists of the same random length.
- Apply `mad` to inputs and compute `mad` by its definition, then compute their difference `diff`
- Type transformation might introduce error.
- As long as the absolute error is smaller than certain threshold (0.00001 in this case) it it acceptable.

**Norm.hs** 
The test won't pass because `mad` cannot handle empty list. 
```
mad :: Vector -> Vector -> Double
mad v1 v2                    
  | DV.length v1 /= 0 = (norm_1 v1 v2) / n 
  | otherwise = 0.0
  where 
    n = fromIntegral . DV.length $ v1 
```
**TestModule.hs**
We also need to update the definition of `mad` to handle empty list.
```
prop'mad :: Int -> Property
prop'mad len = forAll (gen'equal'length'list len) $ prop'
  where
    prop' vt =
      let v1 = fst vt
          v2 = snd vt
          l = fromIntegral . length $ v1
          diff =
            (mad (DV.fromList v1) (DV.fromList v2)) -
            ((\x ->
                 if x == 0
                   then 0.0
                   else (sum $ abs <$> zipWith (-) v1 v2) / x)
               l)
      in collect (l) $ collect (diff) $ abs diff < 0.00001
```
- use `collect` to check the length and diff distribution of all test cases. For example the diff distribution is :
  ```
  76% 0.0
  6% -7.105427357601002e-15
  5% 7.105427357601002e-15
  3% -3.552713678800501e-15
  3% 1.4210854715202004e-14
  2% -1.4210854715202004e-14
  2% -2.1316282072803006e-14
  1% -1.7763568394002505e-15
  1% -2.842170943040401e-14
  1% 1.7763568394002505e-15
  ```

**Norm.hs** 
`minkowskiDistance` 
```
 -- | Minkowski Distance
 -- in this case p is an Integral number for sure.
 minkowskiDistance :: Int -> Vector -> Vector -> Double
 minkowskiDistance p v1 v2 =
   let vDiff = v1 - v2
       absV = DV.map abs vDiff
   in lpnorm p absV
   where
     lpnorm :: Int -> Vector -> Double
     lpnorm 0 vec =
       let vl = -DV.length vec
       in norm_1 $ DV.map (f0 vl) vec
     lpnorm pow vec = nroot pow $ norm_1 $ DV.map (\n -> n ^ p) vec
     nroot
       :: (Integral a, Floating b)
       => a -> b -> b
     nroot 0 _ = 1
     nroot n f = f ** (1 / fromIntegral n)
     f0 :: Int -> Double -> Double
     f0 l v = (2 ^ l) * (v / (1 + v))
```
**TestModule.hs**
When v1 and v2 are of different length, function will always throw exception for all value possible `p`.
```
import Test.QuickCheck.Exception hiding (evaluate)
import Test.QuickCheck.Monadic

describe "Minkowski distance properties" $
  do  it "throw exception when v1 and v2 are of different length " $ property $ prop'mink'diff'list'

prop'mink'diff'list' :: Int -> Property
prop'mink'diff'list' p =
  (p > 0) ==> monadicIO . run $
  do r <-
       tryEvaluate
         $minkowskiDistance
         p
         (DV.fromList [1, 2, 3])
         (DV.fromList [1, 2, 3, 4, 5])
     return $
       case r of
         Left _ -> True
         Right _ -> False
```
- use `tryEvaluate`, `run`, `monadicIO` from `Test.QuickCheck.Monadic` to handle monadic computations.
- usually we could also use `evaluate . force` to test for exception. It is exception handling of `Hspec`.

# .Note 
summary based on [Quickcheck manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) 

>**contents:**
>   1. `Testable & Property` 
>   1. Select desirable element of test set
>       1. `==>`
>       1. `forall`
>   1. Describe test sets distribution
>       1. `classify`
>       1. `collect`
>   1. Generate test set 
>       1. choose
>       1. oneOf
>       1. sized / resize

>  
> ## . **Testable & Property**  
>    ```
>
>    class Testable prop where
>      property :: prop -> Property
>    ...
>    instance [safe] Testable Property
>    instance [safe] Testable prop => Testable (Maybe prop)
>    instance [safe] Testable prop => Testable (Gen prop)
>    instance [safe] Testable Discard
>    instance [safe] Testable Bool
>    instance [safe] (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
>    ...
>  ```
>  ```
>    > :info (==>)
>    (==>) :: Testable prop => Bool -> prop -> Property
>    > :info collect 
>    collect :: (Show a, Testable prop) => a -> prop -> Property
>    > :info classify 
>    classify :: Testable prop => Bool -> String -> prop -> Property
>    > :info forAll
>    forAll ::
>      (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
>    ```
>    - Above functions have type **(Testable prop) => \*-> prop -> Property**
>    - Functions of this type could easily composed together like this
>    ```
>      f1 a==> forAll gen $ prop_f 
>          where prop_f = collect a $ collect b $ classify c s $ prop_imp
>    ```
>    - As long as return type is `Testable`, it can be feed to above functions and get another embeddable value.
>    - `Property` is an intermediate type that cooperate with `Testable` typeclass.
>
> **type initialization**
>  ```
>
>    prop_RevRev xs = reverse (reverse xs) == xs
>      where types = xs::[Int]
>  ```
>  ```
>    prop_rev :: [String] -> Bool
>    prop_rev xs = reverse (reverse xs) == xs
>
>    main = quickCheck prop_RevRev
>  ```
>  - function `reverse` has a property that being implemented on a list twice is equivalent to do nothing to this list.
>  - `quickCheck :: Testable prop => prop -> IO ()`  needs `Testable` input. `Bool` is `Testable`. So the same as function of type: `Testable pro => a -> pro`.
>  - `prop_RevRev :: [Int] -> Bool` & `prop_rev :: [String] -> Bool` are both `Testable`.
>>
>
> - Property must be expressed as haskell function
> - `Polymorphic type a` in `Testable pro => a -> pro` must be initialized as a `monomorphic` type. To provide information to QuickCheck to generate test values. 
>   - `where types = xs :: [Int]` 
>   - `prop_rev :: [String] -> Bool`
> 
>  Without the type declaration above , we will see the following error:
> ```
> * Ambiguous type variable `a0' arising from a use of `quickCheck`...
> ```

>## . Conditional Properties
> `(==>) :: Testable prop => Bool -> prop -> Property`
>
> **Intuition:** 
>  - `prop` on the right side of `==>` is a `Testable` expression. 
>  - `Bool` on the left side of `==>` is a Boolean expression. 
>  - `prop`(right) will not be tested if `Bool`(left) is `false`.
> 
> ```
>  ordered xs = and (zipWith (<=) xs (drop 1 xs))
>  insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs
> 
>  prop_Insert x xs = ordered xs ==> ordered (insert x xs)
>    where types = x::Int
> ```
> - LEFT: if xs is an ordered list
> - RIGHT: function `insert` has such a property: it keeps the order of a list untainted (expressed as: `ordered (insert x xs))`.
> - What `==>` does :
>    - iif `xs` is ordered List, then it can be used to test the property of `insert`.
>
> -  Test case generation continues until 100 cases which do satisfy the condition have been found.
> -  OR
> -  Until an overall limit on the number of test cases is reached (to avoid looping if the condition never holds).

>## . forAll
> ```  
> forAll ::
>   (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
>   	-- Defined in ‘Test.QuickCheck.Property’
> ```
> **Intuition:**
> - `Gen a` is generator of type `a`.
> - `a -> prop` is a `Testable` expression relies on value of type `a`.
> - `forAll` connect Generator and `Testable` expression
>
> ```
>  -- orderedList is a customer generator
>  prop_Insert2 x = forAll orderedList $ \xs -> ordered (insert x xs)
>    where types = x::Int 
>
>  prop_Index_v4 :: (NonEmptyList Integer) -> Property
>  prop_Index_v4 (NonEmpty xs) =
>    forAll (choose (0, length xs-1)) $ \n -> xs !! n == head (drop n xs)
> ```
> - Input :    `Gen a`: generator produce value of type `a`
> - Input :    `a -> prop`: `Testable` function relies on value of type `a`
> - Output: a  `Testable Property`
>
> - Quantify the input (`Gen a`) of `Testable` function (`a -> prop`) 
> - Control the **distribution** of `a`.


> ## Test Case Distribution
> - Test cases are auto generated
> - Distribution of test cases affect the validity of test. 
>
> ### 1. classify 
>```
>  classify :: Testable prop => Bool -> String -> prop -> Property
>```
>**Intuition**
> Test cases that satisfy different conditions can be labeled as different categories. It is good to know proportion of these categories.
> - `Bool` : Conditions being used to classify test cases.
> - `String` : Name a set of test cases that satisfy above condition.
> - `Testable prop`: A `Testable` property.
> Return a `Property` type value enable `embedding` multiple `classify`s together. 
>
>Example:
>```
>  prop_Insert x xs = 
>  	ordered xs ==> 
>  		classify (ordered (x:xs)) "at-head"$
>   		classify (ordered (xs++[x])) "at-tail"$
>  		ordered (insert x xs)
>    where types = x::Int
>```
>```
>  Main> quickCheck prop_Insert
>  OK, passed 100 tests.
>  58% at-head, at-tail.
>  22% at-tail.
>  4% at-head.
>```
> - select test cases with `ordered xs ==>`
> - Embedded classify `Property`s.
> - Define test case category with **Condition** `ordered (x:xs)` and **category name** `"at-head"`.
> - `58% at-head, at-tail` indicates 58% test cases are single element list.
>
> ### 2. collect
>```
>  collect :: (Show a, Testable prop) => a -> prop -> Property
>    	-- Defined in ‘Test.QuickCheck.Property’
> ```
> **Intuition**: 
> - `a`: Map test cases to `Show`able value. 
> - Passed test cases will be categorized by these values.
> - `a` is `Show`able, it will be category name. 
>
>
> Examples:
>```
>  prop_Insert x xs = 
>  	ordered xs ==> collect (length xs)$
>  		       ordered (insert x xs)
>    where types = x::Int
>
>  Main> quickCheck prop_Insert
>  OK, passed 100 tests.
>  58% 0.
>  26% 1.
>  13% 2.
>  3% 3.
>
>  prop_PrimeSum_v2 :: (Positive (Large Int)) -> (Positive (Large Int)) -> Property
>  prop_PrimeSum_v2 (Positive (Large p)) (Positive (Large q)) =
>    p > 2 && q > 2 && isPrime p && isPrime q ==>
>    collect (if p < q then (p, q) else (q, p)) $ even (p + q)
>
>  Main> quickCheck prop_PrimeSum_v2
>  *** Gave up! Passed only 24 tests:
>  16% (3,3)
>  8% (11,41)
>  4% (9413,24019)
>  4% (93479,129917)
>```
> Collecting Data Values. The argument of collect is evaluated in each test case, and the distribution of values is reported. The type of this argument must be in class Show ( from [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html))
>
> ### 3. Category Combination
> - `classify` and `collect` define test case category in different way.>> - We could get joint category by combining combining two methods.
> ```
>  prop_Insert x xs = 
>  	ordered xs ==> 
>  		collect (length xs)$
>  		classify (ordered (x:xs)) "at-head"$
>   		classify (ordered (xs++[x])) "at-tail"$
>  		ordered (insert x xs)
>    where types = x::Int
>
>  Main> quickCheck prop_Insert
>  OK, passed 100 tests.
>  58% 0, at-head, at-tail.
>  22% 1, at-tail.
>  13% 2.
>  4% 1, at-head.
>  3% 3.
> ```
> Conditions are organized just like conditional probability represents context. 
> - In the context of test cases with different length.
> - Then, within a given context, categorize test cases with a new method.


> ## . Test Case Generator
> - QuickCheck can generate test cases of many types.
> - Also choose own generator
>
>```
>   newtype Gen a = MkGen {unGen :: QCGen -> Int -> a }
>   instance [safe] Applicative Gen 
>   instance [safe] Functor Gen 
>   instance [safe] Monad Gen 
>   instance [safe] Testable prop => Testable (Gen prop)
>
>   forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
>
>   choose :: random-1.1:System.Random.Random a => (a, a) -> Gen a
>
>   oneof :: [Gen a] -> Gen a
>   
>   sized :: (Int -> Gen a) -> Gen a
>   resize :: Int -> Gen a -> Gen a
>```
> - Generator is of type `Gen a`.
>   - `Gen` : is a Monad.
> - `forAll` : used together with Default/Customized Generators.
> - `choose` : random choose from an interval .
> - `oneof` : random choose one from a list of Generator.
>
>**Generator Combinators**
>- **vectorOf**
>If `g` is a generator for type `t`, then
>`vectorOf n g generates a list of n `t`s
>```
>vectorOf :: Int -> Gen a -> Gen [a]
>  	-- Defined in ‘Test.QuickCheck.Gen’
>```
>
>- **elements**
>If `xs` is a list, then `elements xs` generates an arbitrary element of `xs`.
>
>```
>elements :: [a] -> Gen a 	-- Defined in ‘Test.QuickCheck.Gen’
>```

# Readings

> online resources:
> - [QuickCheck](http://hackage.haskell.org/package/QuickCheck) Note
> - Haskell Programming from first principles (chapter 12)
> - https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing
> - https://wiki.haskell.org/Introduction_to_QuickCheck1
> - https://wiki.haskell.org/Introduction_to_QuickCheck2
> - https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
> - https://www.fpcomplete.com/blog/2017/01/quickcheck
> - http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
> - https://ocharles.org.uk/posts/2012-12-08-24-days-of-hackage.html
> - http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
> - https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
> - http://matt.might.net/articles/quick-quickcheck/
> - https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html

> Papers:
> - [QuickCheck: a lightweight tool for random testing of Haskell programs](https://doi.org/10.1145/1988042.1988046)
> - [Testing monadic code with QuickCheck](doi.org/10.1145/581690.581696)
> - [QuickCheck Testing for Fun and Profit](www.cs.um.edu.mt/~svrg/FormalMethods/2012-2013/QuickCheck.pdf)

>Hspec
> - Automatic spec discovery[https://hspec.github.io/hspec-discover.html]

## . TODO
1. What is `Property` and `Testable`? How `QuickCheck` works based on these abstraction.
1. `sized` / `resize` : 
1. Arbitrary 
1. Generator Combinator
    1. vectorOf 
    1. elements 
1. The use of `newtype` wrapper [introduction](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html).
1. more examples (maybe a separated doc) .