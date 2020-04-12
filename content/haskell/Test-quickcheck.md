---
title: "Quickcheck"
date: 2020-04-08T17:12:25+08:00
draft: true
---


# . WHY Quickcheck    

- Hspec focus on individual cases 
  - we need to choose input and predict output of the function being tested explicitly manually. 
  ```
    spec :: Spec 
    spec = do 
        describe "Some functionality A" $ 
            it "can do this" (1+1) `shouldBe` 2
  ```
  > function (+) should act as expected.

- Quichcheck focus on `properties`
  - Function being tested should satisfy certain property on a given set. 
  ```
    prop_RevRev xs = reverse (reverse xs) == xs
      where types = xs::[Int]
  ```
  > function `reverse` should satisfy this property. It is that reversing a list twice equivalent to doing nothing to this list at all.
  
  Quickcheck provide one possibility to test over as many use cases of a function as possible. It could offer: 
    1. Auto generated input.
    1. Auxiliary utilities to help describe the set of test cases. 




# . Quickcheck Basic Idea  

```
class Testable prop where
  property :: prop -> Property
  propertyForAllShrinkShow :: Gen a
                              -> (a -> [a]) -> (a -> [String]) -> (a -> prop) -> Property
  {-# MINIMAL property #-}
  	-- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable Property
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable prop => Testable (Maybe prop)
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable prop => Testable (Gen prop)
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable Discard
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable Bool
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] (Arbitrary a, Show a, Testable prop) =>
                Testable (a -> prop)
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable ()
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable HUnit-1.6.0.0:Test.HUnit.Lang.Assertion
  -- Defined in ‘quickcheck-io-0.2.0:Test.QuickCheck.IO’
```

```
quickCheck :: Testable prop => prop -> IO ()
```

```
quickCheckWith :: Testable prop => Args -> prop -> IO ()
```

```
Testable prop => prop -> IO ()
```
# . Use Quickcheck in stack project :
- examples in folder `tests/Quicknote`, the same as `source-dirs` in package.yaml
- main model in file `Examples.hs`, the same as `main` in package.yaml
- The model name of file `Examples.hs` must be `Main` rather than `Example` or `Quicknote.Example`.
  - The model name of file which is designated in `main` field in the package.yaml must be `Main`.
  - If there is more than one file in `tests/Quicknote` there mode name is the form `Quicknote.xxxx`.



# . Sample reminder
```
import Test.QuickCheck 

prop_rev :: [a] -> Bool
prop_rev xs = reverse (reverse xs) == xs

main = quickCheck prop_RevRev
```
The target function we would like to test is `reverse`.   
The property of this target function is: If we apply this function twice `reverse (reverse xs)`, we will get the original list `xs`.  
This property is being described as `reverse (reverse xs) == xs`  

QuickCheck generates number of random values of type `a` and feed to function `prop_rev`.   

The most commonly being used function is of type:
```
auxiFunc :: a -> Bool
```
This function is an instance of TypeClass `Testable`.

**TODO**: 
  - Example `forAll`:
  - Examples of following Note.


# .Note 
summary based on [Quickcheck manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) 

It is about : 
>   1. `Property` 
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
> ## . **Properties**  
>>  ```
>>    newtype Property = MkProperty { unProperty :: Gen Prop }
>>
>>    class Testable prop where
>>      property :: prop -> Property
>>    ...
>>    instance [safe] Testable Property
>>    instance [safe] Testable prop => Testable (Maybe prop)
>>    instance [safe] Testable prop => Testable (Gen prop)
>>    instance [safe] Testable Discard
>>    instance [safe] Testable Bool
>>    instance [safe] (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
>>    ...
>>  ```
>>  **Intuition** : 
>>    - Many types are `Testable`, `Bool` or function of type `(Testable prop) => a -> prop`.
>>    - `Property` is common wrapper all all types that are `Testable`.
>>      - **TODO** : 
>>            1. more details (maybe a separated doc)
>>            2. Why is this wrapper necessary .
> 
>>  ```
>>    prop_RevRev xs = reverse (reverse xs) == xs
>>      where types = xs::[Int]
>>  ```
>>  ```
>>    prop_rev :: [String] -> Bool
>>    prop_rev xs = reverse (reverse xs) == xs
>>
>>    main = quickCheck prop_RevRev
>>  ```
>>  - function `reverse` has a property that being implemented on a list twice is equivalent to do nothing to this list.
>>  - `quickCheck :: Testable prop => prop -> IO ()`  needs `Testable` input. `Bool` is `Testable`. So the same as function of type: `Testable pro => a -> pro`.
>>  - `prop_RevRev :: [Int] -> Bool` & `prop_rev :: [String] -> Bool` are both `Testable`.
> - Property must be expressed as haskell function
> - `Polymorphic type a` in `Testable pro => a -> pro` must be initialized as a `monomorphic` type. To provide information to QuickCheck to generate test values. 
>>   - `where types = xs :: [Int]` 
>>   - `prop_rev :: [String] -> Bool`
>> 
>>  Without the type declaration above , we will see the following error:
>> ```
>> * Ambiguous type variable `a0' arising from a use of `quickCheck`...
>> ```

>## . Conditional Properties
>> `(==>) :: Testable prop => Bool -> prop -> Property`
>>
>> **Intuition:** 
>>  - `prop` on the right side of `==>` is a `Testable` expression. 
>>  - `Bool` on the left side of `==>` is a Boolean expression. 
>>  - `prop`(right) will not be tested if `Bool`(left) is `false`.
> 
>> ```
>>  ordered xs = and (zipWith (<=) xs (drop 1 xs))
>>  insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs
>> 
>>  prop_Insert x xs = ordered xs ==> ordered (insert x xs)
>>    where types = x::Int
>> ```
>> - LEFT: if xs is an ordered list
>> - RIGHT: function `insert` has such a property: it keeps the order of a list untainted (expressed as: `ordered (insert x xs))`.
>> - What `==>` does :
>>    - iif `xs` is ordered List, then it can be used to test the property of `insert`.
> 
> -  Test case generation continues until 100 cases which do satisfy the condition have been found.
> -  OR
> -  Until an overall limit on the number of test cases is reached (to avoid looping if the condition never holds).

>## . forAll
>> ```  
>> forAll ::
>>   (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
>>   	-- Defined in ‘Test.QuickCheck.Property’
>> ```
>> **Intuition:**
>> - `Gen a` is generator of type `a`.
>> - `a -> prop` is a `Testable` expression relies on value of type `a`.
>> - `forAll` connect Generator and `Testable` expression
>
>> ```
>>  -- orderedList is a customer generator
>>  prop_Insert2 x = forAll orderedList $ \xs -> ordered (insert x xs)
>>    where types = x::Int 
>>
>>  prop_Index_v4 :: (NonEmptyList Integer) -> Property
>>  prop_Index_v4 (NonEmpty xs) =
>>    forAll (choose (0, length xs-1)) $ \n -> xs !! n == head (drop n xs)
>> ```
>> - Input :    `Gen a`: generator produce value of type `a`
>> - Input :    `a -> prop`: `Testable` function relies on value of type `a`
>> - Output: a  `Testable Property`
> 
> - Quantify the input (`Gen a`) of `Testable` function (`a -> prop`) 
> - Control the **distribution** of `a`.


> ## Test Case Distribution
> - Test cases are auto generated
> - Distribution of test cases affect the validity of test. 
>
> ### 1. classify 
>>```
>>  classify :: Testable prop => Bool -> String -> prop -> Property
>>```
>>**Intuition**
>> Test cases that satisfy different conditions can be labeled as different categories. It is good to know proportion of these categories.
>> - `Bool` : Conditions being used to classify test cases.
>> - `String` : Name a set of test cases that satisfy above condition.
>> - `Testable prop`: A `Testable` property.
>> Return a `Property` type value enable `embedding` multiple `classify`s together. 
>
>>Example:
>>```
>>  prop_Insert x xs = 
>>  	ordered xs ==> 
>>  		classify (ordered (x:xs)) "at-head"$
>>   		classify (ordered (xs++[x])) "at-tail"$
>>  		ordered (insert x xs)
>>    where types = x::Int
>>```
>>```
>>  Main> quickCheck prop_Insert
>>  OK, passed 100 tests.
>>  58% at-head, at-tail.
>>  22% at-tail.
>>  4% at-head.
>>```
>> - select test cases with `ordered xs ==>`
>> - Embedded classify `Property`s.
>> - Define test case category with **Condition** `ordered (x:xs)` and **category name** `"at-head"`.
>> - `58% at-head, at-tail` indicates 58% test cases are single element list.
> 
> ### 2. collect
>>```
>>  collect :: (Show a, Testable prop) => a -> prop -> Property
>>    	-- Defined in ‘Test.QuickCheck.Property’
>> ```
>> **Intuition**: 
>> - `a`: Map test cases to `Show`able value. 
>> - Passed test cases will be categorized by these values.
>> - `a` is `Show`able, it will be category name. 
>>
> 
>> Examples:
>>```
>>  prop_Insert x xs = 
>>  	ordered xs ==> collect (length xs)$
>>  		       ordered (insert x xs)
>>    where types = x::Int
>>
>>  Main> quickCheck prop_Insert
>>  OK, passed 100 tests.
>>  58% 0.
>>  26% 1.
>>  13% 2.
>>  3% 3.
>>
>>  prop_PrimeSum_v2 :: (Positive (Large Int)) -> (Positive (Large Int)) -> Property
>>  prop_PrimeSum_v2 (Positive (Large p)) (Positive (Large q)) =
>>    p > 2 && q > 2 && isPrime p && isPrime q ==>
>>    collect (if p < q then (p, q) else (q, p)) $ even (p + q)
>>
>>  Main> quickCheck prop_PrimeSum_v2
>>  *** Gave up! Passed only 24 tests:
>>  16% (3,3)
>>  8% (11,41)
>>  4% (9413,24019)
>>  4% (93479,129917)
>>```
>> Collecting Data Values. The argument of collect is evaluated in each test case, and the distribution of values is reported. The type of this argument must be in class Show ( from [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html))
>
> ### 3. Category Combination
>> - `classify` and `collect` define test case category in different way.>> - We could get joint category by combining combining two methods.
>> ```
>>  prop_Insert x xs = 
>>  	ordered xs ==> 
>>  		collect (length xs)$
>>  		classify (ordered (x:xs)) "at-head"$
>>   		classify (ordered (xs++[x])) "at-tail"$
>>  		ordered (insert x xs)
>>    where types = x::Int
>>
>>  Main> quickCheck prop_Insert
>>  OK, passed 100 tests.
>>  58% 0, at-head, at-tail.
>>  22% 1, at-tail.
>>  13% 2.
>>  4% 1, at-head.
>>  3% 3.
>> ```
>> Conditions are organized just like conditional probability represents context. 
>> - In the context of test cases with different length.
>> - Then, within a given context, categorize test cases with a new method.
>

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
> ## . TODO
> - `sized` / `resize` : 
> - Arbitrary 
> - Generator Combinator
>   - vectorOf 
>   - elements 
> - The use of `newtype` wrapper [introduction](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html).


>- **Generator Combinators**
>>- **vectorOf**
>>If `g` is a generator for type `t`, then
>>`vectorOf n g generates a list of n `t`s
>>```
>>vectorOf :: Int -> Gen a -> Gen [a]
>>  	-- Defined in ‘Test.QuickCheck.Gen’
>>```
>
>>- **elements**
>>If `xs` is a list, then `elements xs` generates an arbitrary element of `xs`.
>>
>>```
>>elements :: [a] -> Gen a 	-- Defined in ‘Test.QuickCheck.Gen’
>>```

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