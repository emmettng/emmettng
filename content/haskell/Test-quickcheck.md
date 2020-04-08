---
title: "Test Quickcheck"
date: 2020-04-08T17:12:25+08:00
draft: true
---

## 1.Hspec
Hspec name is `Model+Spec.hs`

## 2.Quickcheck    
Quickcheck stack project note:
- examples in folder `tests/Quicknote`, the same as `source-dirs` in package.yaml
- main model in file `Examples.hs`, the same as `main` in package.yaml
- The model name of file `Examples.hs` must be `Main` rather than `Example` or `Quicknote.Example`.
  - The model name of file which is designated in `main` field in the package.yaml must be `Main`.
  - If there is more than one file in `tests/Quicknote` there mode name is the form `Quicknote.xxxx`.

```
Similarity
  L1-norm based. Property tests:

```

```
 quickCheck :: quickCheck :: Testable prop => prop -> IO ()
```

```
quickCheckWith :: quickCheckWith :: Testable prop => Args -> prop -> IO ()
```

```
verboseCheck :: Testable prop => prop -> IO ()
```

#### 2.1 Sample reminder
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

## 2. Typical test flow
TODO 
with necessary chart    
condition, generator, sized etc...

## 3. Note
>  
> - **Properties**  
> `Polymorphic` properties, such as the function `prop_rev` in simple remind, must be restricted to a particular type to be used for testing. Otherwise, there will be no clue for QuickCheck to generate test values.
> ```
> prop_rev :: [String] -> Bool
> prop_rev xs = reverse (reverse xs) == xs
> 
> main = quickCheck prop_RevRev
> ```
> Without the type declaration in the first line, there will be following error:
> ```
> * Ambiguous type variable `a0' arising from a use of `quickCheck`...
> ```

> - **==>**  
> **Condition**
> ```
> (==>) :: Testable prop => Bool -> prop -> Property
>   	-- Defined in ‘Test.QuickCheck.Property’
> ```
> ```
> fa a ==> auxiFunc
> ```
> - `fa :: [a] -> Bool` : A filter function, discards auto generated test cases which do not satisfy the condition, Test case generation continues until 100 cases which do satisfy the condition have been found, or until an overall limit on the number of test cases is reached (to avoid looping if the condition never holds).

> - **forAll**    
> ```  
> forAll ::
>   (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
>   	-- Defined in ‘Test.QuickCheck.Property’
> ```
>
>Quantified Properties. The first argument of `forAll` is a test data generator; by supplying a custom generator, instead of using the default generator for that type, it is possible to control the **distribution** of test data. 


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


# [QuickCheck](http://hackage.haskell.org/package/QuickCheck) Note
> resources:
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