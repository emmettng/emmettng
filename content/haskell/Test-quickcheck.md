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
# . Use Quickcheck in stack project along:
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



# .Note 
summary based on [Quickcheck manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
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
>>    - Many types are `Testable`, `Bool` or function of type `a -> prop`.
>>    - `Property` is buffer all all types that are `Testable`.
>>      - **TODO** : more details (maybe a separated doc)
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
>> - LEFT: xs is an ordered list
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


> **Observing Test Case Distribution**
> It is important to be aware of the distribution of test cases: if test data is not well distributed then conclusions drawn from the test results may be invalid. In particular, the ==> operator can skew the distribution of test data badly, since only test data which satisfies the given condition is used.
>> -  **classify**    
>> ```
>> classify :: Testable prop => Bool -> String -> prop -> Property
>>   	-- Defined in ‘Test.QuickCheck.Property’
>> ```
>> `Bool` : Identify certain category.  
>> `String`: Name that category.  
>> Classifying Test Cases. Test cases satisfying the condition are assigned the classification given, and the distribution of classifications is reported after testing. In this case the result is
>> 
>
> 
>> - **collect**    `
>> ```
>> collect :: (Show a, Testable prop) => a -> prop -> Property
>>  	-- Defined in ‘Test.QuickCheck.Property’
>> ```
>>
>> Collecting Data Values. The argument of collect is evaluated in each test case, and the distribution of values is reported. The type of this argument must be in class Show
>> 

> 
> `classify` and `collect` may be combined in any way. All the observations of each test case are combined, and the distribution of these combinations is reported. For example, testing the property
> ```
> examples: TO DO
> ```

> Test Data Generators: The Type Gen  
> Test data is produced by test data generators. QuickCheck defines default generators for most types, but you can use your own with forAll, and will need to define your own generators for any new types you introduce.
> Generators have types of the form Gen a; this is a generator for values of type a. The type Gen is a monad, so Haskell's do-syntax and standard monadic functions can be used to define generators.
>> - **choose**
>> which makes a random choice of a value from an interval, with a uniform distribution.
>> ```
>> choose :: random-1.1:System.Random.Random a => (a, a) -> Gen a
>>   	-- Defined in ‘Test.QuickCheck.Gen’
>> ```
>>  For example, to make a random choice between the elements of a list
>>  ```
>>  do i<-choose (0,length xs-1)
>>    return (xs!!i)
>>  ```
> 
>> - **oneof** 
>> Choosing between alternatives. A generator may take the form
>> 	```oneof <list of generators>```
>> which chooses among the generators in the list with equal probability. 
>> ```
>> oneof:: [Gen a] -> Gen a 	-- Defined in ‘Test.QuickCheck.Gen’
>> ```
> 
>> - **frequency**
>> We can control the distribution of results using the function.Frequency chooses a generator from the list randomly, but weights the probability of choosing each alternative by the factor given.
>> ```
>> frequency :: [(Int, Gen a)] -> Gen a
>>   	-- Defined in ‘Test.QuickCheck.Gen’
>> ```
>> for example, 
>> ```frequency [(2,return True), (1,return False)]```
>> generates True two thirds of the time.
>
>
>> - **Generating Recursive Data Types**
>> Generators for recursive data types are easy to express using oneof or frequency to choose between constructors, and Haskell's standard monadic combinators to form a generator for each case. For example, if the type of trees is defined by. For example:
>> ```
>> prime
>> ```
>> However, there is always a risk that a recursive generator like this may fail to terminate, or produce very large results. To avoid this, recursive generators should always use the **size control mechanism**. For example:
>> ```
>> Tree example
>> ```




> - **sized**
> Test data generators have an implicit size parameter; quickCheck begins by generating small test cases, and gradually increases the size as testing progresses. Different test data generators interpret the size parameter in different ways: some ignore it, while the list generator, for example, interprets it as an upper bound on the length of generated lists. You are free to use it as you wish to control your own test data generators.
> ```
> sized :: (Int -> Gen a) -> Gen a
>   	-- Defined in ‘Test.QuickCheck.Gen’
> ```
> example:
> ```
> TODO
> ```

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

>- **Tip: Using newtype**
>want a different distribution
>Of course, you can write a custom test data generator for variable names, maybe choosing randomly from a small set, and try to remember to use it wherever a string plays the role of a name. But this is error prone. Much better is to define a new type of names, isomorphic to String, and make your custom generator the default for it. For example,
>
>newtype Name = Name String
>
>instance Arbitrary Name where
>  `arbitrary = oneof ["a", "b", "c", "d", "e"]`


## 2. Typical test flow
TODO 
with necessary chart    
condition, generator, sized etc...


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