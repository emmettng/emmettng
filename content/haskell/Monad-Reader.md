---
title: "Monad: Reader"
date: 2019-11-18T00:18:29+08:00
draft: false 
---
> This summary follows the minimum useable principle.

#### Readings 
- [Learn you a haskell](http://learnyouahaskell.com/for-a-few-monads-more)
- [hackage: transformers ](http://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Reader.html)


### Monadic Semantics

Several functions depend on the same `env` information. In other words, they are all readers of the same environment information. Many `Reader`s composed together by `>>=` or `>=>` to be a functional unit. The `env` information is being passed implicitly through the chain of computation.

```
type ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

- Function `ask` introduce the `env` information into the Reader Monad
- `local` (withReaderT) alter the `env` information temporarily.
- `asks` convert a function from `env` to other type to a Reader Monad.
- Usually, one `ask` will related to one Reader Monad that represent a function depends on `env` information.

### ask
```
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return
    -- return :: r -> m r , return in this case is defined for 'm'
```

### local
```
local
local
    :: (r -> r)         -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r m a
local = withReaderT
```

### withReaderT
```
withReaderT
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f
```

### asks
```
asks
asks :: (Monad m)
    => (r -> a)         -- ^ The selector function to apply to the environment.
    -> ReaderT r m a
asks f = ReaderT (return . f)
{-# INLINE asks #-}
```

#### Common usage
1. use `ask` to introduce the `env` into computation. (almost compulsory, asks is rarely being used)
2. so we could construct functions of type `a -> Reader r b` or just `Reader r b`. (compulsory)
3. `local` or `withReaderT` alter environment (optional)
5. `runReader` or `runreaderT` to unwrap functions. (compulsory)
6. Feed the `env` information (compulsory)
   
#### Intuition:
Pass Env/Context/Configuration information through a chain of operations that depend on same set of configurations.

**terms** 

- function chain (composition):        
  A sequence of functions that compose.    
  - `A :: a -> b`
  - `B :: b -> c`
  - `C :: c -> d`
  - `D :: d-> e`

    A chain of functions above:         
    ` D . C . B . A :: a -> e`
  
**intuitation recap**

   - pass environment information `env` through all components of a function chain. 
   - every functions in this function chain use this `env` or part of this `env`.
   - Common usage include:  
  
        > ask  :: introducing `env` into function chain.    
        > local :: temporarily change `env` value or type.     
        > asks :: swiftly turn a function of type `env -> result` into this monad.    
        > runreaderT :: Get the functions (chain of operations), so it can be evaluated by feeding in a `env`.  

## Examples 
***0.necessary imports***
```
-- Example One imports
import           Control.Monad.Trans.Reader
import           GHC.Float

-- Example Two imports
import           Control.Monad.IO.Class
import           Control.Monad    -- for the use of kleisli arrow ( >=> )

-- Staging Example imports
import           GHC.Float
import           Data.Store
```


### 1.Example one 

- A chain of Core functions.
- Intuition:
    - `an initial input -> a chain of functions`     
       which inputs and outputs aligned and represented by a Reader Monad.
    - Every operation in this function chain shares the same `env` informatoin.
        - CoreInputType -> Reader Env ChainOutputType 
        - The `env` being passed down through the core operation chain.

- core function 1 `:: Int -> Int`
    ```
    a1 :: Int -> Reader Int Int
    a1 n = do
        env <- ask           -- get Enviroment information
        return $ n + env
    ```
- core function 2 :: `Int -> Float`
    ```
    a2 :: Int -> Reader Int Float
    a2 n = do
        env <- ask          -- get Enviroment information
        let fn = fromIntegral n
        return $ fromIntegral env / fn
    ```
- core function 3 :: `Float -> Double`
    ```
    a3 :: Float -> Reader Int Double
    a3 f = do
        env <- ask          -- get Enviroment information
        let fn = fromIntegral env
        return $ float2Double $ fn * f
    ```
- A Chain of core functions. 
    - Input is a1 of type `Int`
    - chain output is a3 of type `Double`
    - Only when input and `env` being provided, the result can be produced.
    ```
    chainA :: Int -> Reader Int Double
    chainA n = do
        t1 <- a1 n          -- core operaiton 1
        t2 <- a2 t1         -- core operaiton 2
        a3 t2               -- core operation 3
    ```
    **alternative:** `kleisli arrow`
    ```
    chainA' :: Int -> Reader Int Double
    chainA' = a1 >=> a2 >=> a3
    ```

***example output***
```
> let p1 = chainA 10
> runReader p1 $ 2
> 0.3333333432674408
```
In this example `env` is 2 and `n` (the initial input) is 10 so :
```
t1 = 10 + 2 , t2 = 2 / 12,  t3 = 2* (2 / 12)  =  0.33333333
```
### 2.Example Two
- A chian of functions depend only on `Env` information.
- Intuition :
    - Sequence operation depends only on Environment. 


```
b1 :: (MonadIO m) => ReaderT String m Int
b1 = do
    env <- ask           -- get Environment information
    n   <- liftIO getLine
    let n1 = read env
        n2 = read n
    return $ n1 + n2
```
```
b2 :: (MonadIO m) => ReaderT String m Float
b2 = do
    env <- ask          -- get Environment information
    n   <- liftIO getLine
    let n1 = read env
        n2 = read n
    return $ n1 / n2
```
```
b3 :: (MonadIO m) => ReaderT String m Double
b3 = do
    env <- ask          -- get Enviroment information
    n   <- liftIO getLine
    let n1 = read env
        n2 = read n
    return $ n1 * n2
```
- function b1, b2, b3 depend on same `env` of type String

```
tryB :: ReaderT String IO Float
tryB = do
    t1 <- b1
    t2 <- b2
    t3 <- b3
    return $ fromIntegral t1 + t2 + double2Float t3
```
- `runReaderT tryB` --> function that take a Env and produce the result of this composition chain.
```
> :info tryB
>   tryB :: ReaderT String IO Float
> let f = runReaderT tryB
> :info f
>   f :: String -> IO Float 	-- Defined at <interactive>:6:5
```

``` 
> runReaderT :: ReaderT  --> function :: Env -> Result
> let p = runReaderT tryB
> r <- p "10"
> 1
> 2
> 3
>  r
> 46.0
```

In this example `env` is `10` and it is of type string. Each function take another input from the commond line.
- `1+10  + 10/2 + 3*10 = 46`

### 3.Example Three 
```
local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
```
- Intuition :
    - local change env to the same type .
    - withReaderT change env to another type .
- **IMPORTANT:** The modification only effects **temporarily**. That is the reason why the name is `local`.

```
changeEnv :: String -> Float
changeEnv s = read s + 100
```

This reader `c1` is different from `b1` or `b2` or `b3`. Its `env` information is of type `Float`.
```
c1 :: (Monad m) => ReaderT Float m Float
c1 = do
    env <- ask
    return $ env * 5
```
```
tryC :: ReaderT String IO Float
tryC = do
    t1  <- b1
    t2  <- b2
    tc1 <- withReaderT changeEnv c1
    t3  <- b3
    return $ fromIntegral t1 + t2 + double2Float t3 + tc1
```
```
> let p = runReaderT tryC
> r <- p "10"   -- env is "10"
>1        -- b1 readin 1  
>2        -- b2 readin 2  ; computation c1 
>3        -- b3 readin 3 
> r
-- 346.0
```
The result is `t1+t2+t3+tc1`
```
1+10 + 10/2 + 10*3 + (10+100)*5 = 596
```

> - local modify the value of Env temporarily.
> - withReaderT is more general, it could modify the type of Env.
> The definition of withReaderT guarantee the computation stays in the original `env` context.

```
ReaderT r' m a
local
    :: (r -> r)         -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r m a
local = withReaderT
```
```
withReaderT
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f
```
### 4.Example Four 
```
asks :: (Monad m) => (r -> a) -> ReaderT r m a 
asks f = ReaderT (return . f)
```
- Intuition:
    - convert an simple function into ReaderT. 

```
simpleFunc :: String -> Float
simpleFunc s = 1000 + read s
```
```
tryD :: ReaderT String IO Float
tryD = do
    t1  <- b1
    t2  <- b2
    tc1 <- withReaderT changeEnv c1
    t3  <- b3
    td1 <- asks simpleFunc
    return $ fromIntegral t1 + t2 + double2Float t3 + tc1 + td1
```
```
> let p = runReaderT tryD 
> r <- p "10"
>1
>2
>3
> r
>1606.0
```
This example is: `t1 + t2 + t3 + tc1 + td1`
```
1+10 + 10/2 +  10*3 + 110 * 5 + (10+1000)= 1606
```
***Notes***

- Given a function of type ` :: r -> a`, we could use `asks` to convert it monad `ReaderT r m a`. However, it is not certainly applicable the other way around.     
It could be impossible to factorize a function of type `ReaderT r m a` as combinations of `asks` and ` f `. 
    - The implementation of `asks` is `asks f = ReaderT (return . f)`
    - In `ReaderT r m a`, if `m` is `Either` or `IO`, there will be no hope for `return . f` to produce `Computational Context` Information of `m`.
    - In this case, `m` usually related to more than one Type and each Type contains more than one possible value. 


### 5.Complex Example : Staging    

#### Business logic :

1. Core functions: 
   - `f1 :: a -> b `
   - `f2 :: b -> c `
   - `f3 :: c -> d `
   - `f4 :: d -> e `
   - `f5 :: e -> f `
  
    `chainF :: a -> f `     
    `chainF = f5 . f4 . f3 . f2 . f1`

2. Core Functions depends on `Config` information. So :
   - `f1 :: a -> e -> b `
   - `f2 :: b -> e -> c `
   - `f3 :: c -> e -> d `
   - `f4 :: d -> e -> e `
   - `f5 :: e -> e -> f `
  
    `chainF :: e -> a -> f `     

    We could rewrite `f1` to `f5` as:
   - `f1 :: a -> Reader e b` 
   - `f2 :: b -> Reader e c `
   - `f3 :: c -> Reader e d `
   - `f4 :: d -> Reader e e `
   - `f5 :: e -> Reader e f `

    `chainF :: a -> Reader e f `     

    ``` 
    chainF a = do 
        e <- ask 
        b <- f1 a 
        c <- f1 b 
        d <- f1 c 
        e <- f1 d 
        f <- f1 e 
        return f
    ```
    or  

    ` chainF = f1 >=> f2 >=> f3 >=> f4 >=> f5 `

3. The computation of each **core** function could be very complicated and time consuming, so I would like to save the result of every step on the disk. If corresponding `config` doesn't change, the next time when `chainF` is running we can simple deserialize existed result and provide to the following functions.
 
***4. ReaderT implementations:***



1. The `Env` contains information for a chain of computations, functions for serialization and deserialization. This `Env` could be initialized from `Conifg` information.
```
type Serializer = forall a. (Store a) =>
                            a -> FilePath -> IO ()

type Deserializer = forall a. (Store a) =>
                              FilePath -> IO a

data Env = Env
  { sf1Env :: String
  , sf2Env :: Int
  , sf3Env :: Float
  , sf4Env :: Double
  , sf5Env :: Int
  , serializer :: Serializer
  , deserializer :: Deserializer
  }
```

2. StageCore function relies on `env` information 
    - `type StageCore a = ReaderT Env IO a`

3. Functions that fully or partially depends on `env` information.
```
-- sf1
--   :: (Monad m)
--   => ReaderT Env m Int
sf1 :: StageCore Int
sf1 = do
  env <- ask
  return $ length . sf1Env $ env
```
```
sf2 :: Int -> StageCore Float
sf2 arg2 = do
  env <- ask
  return $ fromIntegral $ sf2Env env + arg2
```
```
sf3 :: Float -> StageCore Double
sf3 arg3 = do
  env <- ask
  return $ float2Double $ sf3Env env + arg3
```
```
sf4 :: Double -> StageCore String
sf4 arg4 = do
  env <- ask
  return $ show $ sf4Env env + arg4
```
```
sf5 :: StageCore Int
sf5 = do
  env <- ask
  return $ 100 + sf5Env env
```

```
coreChain :: () -> ReaderT Env IO Int
coreChain = (\_ -> sf1) >=> sf2 >=> sf3 >=> sf4 >=> (\_ -> sf5)
```

4. Now we would like to:
    1. Serialize the output of each core functions to the disk.
    2. If env information is new , we do the calculation.
    3. If previous calculation is new, we do this calculation.
    4. otherwise, we deserialize what we saved on the disk.

5. we need to name each stage/computation 
`type StageName = String`

6. information need for core function `a -> b` now need to embellished with a `Boolean` type to indicate the serialization status.    
`type Core a = (a, Bool)`

7. We use Data.Store to serialize output of each core function    
   StageName and Env defines this boolen value 
```
checkSerialization :: StageName -> StageCore (Core FilePath)
checkSerialization = undefined
```

8. Function `staging` take:
    1. a `StageName` to identify this computation.
    2. a core function of type ` a -> StageCore b `.
    3. produce a new function ` Core a -> StageCore (Core b)   
This would be make more sense, 
    - embellish input/output of the original core-function with the Bool information `(a, Bool) -> StageCore (b,Bool) == Core a -> StageCore (Core b)`
    - this bool information is being used for helping following operation to decide whether to do the computation or just desrialize from dist.
    - a `StageName` being used to identify stage and influence the Boolean information.
```
staging
  :: (Store b)
  => StageName -> (a -> StageCore b) -> Core a -> StageCore (Core b)
staging stageName coreFunc (coreInput, preStatus) = do
  env <- ask
  let savetoDisk = serializer env
      readDisk = deserializer env
  (serializationPath, done) <- checkSerialization stageName
  if done && preStatus
    then do
      ds <- liftIO $ readDisk serializationPath
      return (ds, True)
    else do
      coreOutput <- coreFunc coreInput
      liftIO $ savetoDisk coreOutput serializationPath
      return (coreOutput, False)
```
9. Finally, `Staging` could be used like this:
```
stageChain =
  staging "stageFunction_1" (\_ -> sf1) >=>
  staging "stageFunction_2" sf2 >=>
  staging "stageFunction_3" sf3 >=> 
  staging "stageFunction_4" sf4 >=> 
  staging "stageFunction_5" (\_ -> sf5)
```


### 6. ReaderT Design Pattern 
> - The famous 23 [design pattern](https://en.wikipedia.org/wiki/Design_Patterns) is almost perfect except for the suspicious number **`23`**.
> - This number **`23`** is not as convincing as the number **`42`**.
> - With highly abstracted expression ability, the design patterns of Haskell iImplementation would be more like a personal preference or a hobby rather than some task specific best practices.

#### Readings 
- [Monad Reader](../monad-reader)
- [fp complete: ReaderT](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) (compulsory)
- [haskell in production](http://felixmulder.com/writing.html)

#### Desire 
- Want to know what is this function about from the type signature.

#### issues and solutions

1. Information in the function type signature: 
     - **Obvious**: core function is from `Double` to `String`
     - **Blur**: The meaning of StageCore. Needs reference of `StageCore`
     - **No**: which part of `env` affects this function.
  ```
      sf4 :: Double -> StageCore String     
      sf4 arg4 = do                              
        env <- ask                                                      
        return $ show $ sf4Env env + arg4                              
  ```
2. Use `Has` type class to constraint only necessary information to be used in `env` 
  ```
    type NewStage r a = ReaderT r IO a    

    class HasEnv4 a  where
      getSF4env :: a -> Double
  ```
  `NewStage Double String` enabling the use of `Double` in test. So we don't need to construct a meaningless `env` .
  ```
    instance HasEnv4 Double where
      getSF4env = id
  ```
  ```
    instance HasEnv4 Env where
      getSF4env = sf4Env
  ```
  ```
   sf4N                                  
     :: (HasEnv4 r)                           
     => Double -> NewStage r String                                  
   sf4N arg4 = do                                                   
     env <- ask                                                   
     return $ show $ getSF4env env + arg4
  ```

#### Example 
- The above `Complex example` is the main body of ReaderT design pattern.
- Initialize `Env` using [configurator](../package-configurator).
- Each computation in the computation chain only relies on part of `Env`. Fix this by using `Has` typeclass.
```
data Env = Env
  { sf1Env :: String                --being used by sf1 only
  , sf2Env :: Int                   --being used by sf2 only
  , sf3Env :: Float                 --being used by sf3 only
  , sf4Env :: Double                --being used by sf4 only
  , sf5Env :: Int                   --being used by sf5 only
  , serializer :: Serializer
  , deserializer :: Deserializer
  }
```

***1. Define type class***
```
--TypeClass for retrieving sf1Env from Env
class HasEnv1 a  where
  getSF1env :: a -> String

instance HasEnv1 String where
  getSF1env = id

instance HasEnv1 Env where
  getSF1env = sf1Env

```
```
-- TypeClass for retrieving sf2Env from Env
class HasEnv2 a  where
  getSF2env :: a -> Int

instance HasEnv2 Int where
  getSF2env = id

instance HasEnv2 Env where
  getSF2env = sf2Env

```
```
-- TypeClass for retrieving sf3Env from Env
class HasEnv3 a  where
  getSF3env :: a -> Float

instance HasEnv3 Float where
  getSF3env = id

instance HasEnv3 Env where
  getSF3env = sf3Env
```
```

-- TypeClass for retrieving sf4Env from Env
class HasEnv4 a  where
  getSF4env :: a -> Double

instance HasEnv4 Double where
  getSF4env = id

instance HasEnv4 Env where
  getSF4env = sf4Env
```
```
-- TypeClass for retrieving sf5Env from Env
class HasEnv5 a  where        
  getSF5env :: a -> Int        

instance HasEnv5 Int where        
  getSF5env = id        

instance HasEnv5 Env where        
  getSF5env = sf5Env        
```
Two instances were declared, with the help of the first function `getEnv = id`, we could test function depends on this typeclass easily (defined as follow). 

***2.define corresponding functions***
```
-- | Then function sf1 to sf5 could rewrite as follow        


type NewStage r a = ReaderT r IO a        

sf1N        
  :: (HasEnv1 r)        
  => NewStage r Int        
sf1N = do        
  env <- ask        
  return $length . getSF1env $ env        

sf2N        
  :: (HasEnv2 r)
  => Int -> NewStage r Float
sf2N arg2 = do
  env <- ask
  return $ fromIntegral $ getSF2env env + arg2

sf3N
  :: (HasEnv3 r)
  => Float -> NewStage r Double
sf3N arg3 = do
  env <- ask
  return $ float2Double $ getSF3env env + arg3

sf4N
  :: (HasEnv4 r)
  => Double -> NewStage r String
sf4N arg4 = do
  env <- ask
  return $ show $ getSF4env env + arg4
sf5N
  :: (HasEnv5 r)
  => NewStage r Int
sf5N = do
  env <- ask
  return $ 100 + getSF5env env
```
In this case:        
1. function associate with meaningful type signatures        
2. It can be test very easily.        
3. The type of the core computation could stay the same as before, but we updated the participant component with more flexibility.

***3. new computation chain***

Type signature stays the same as before
```
NewStage Env Int = StageCore Int = ReaderT Env IO Int
```
```
newCoreChain :: () -> NewStage Env Int
newCoreChain = (\_ -> sf1N) >=> sf2N >=> sf3N >=> sf4N >=> (\_ -> sf5N)
-- newCoreChain = (\_ -> sf1) >=> sf2 >=> sf3 >=> sf4 >=> (\_ -> sf5)
```

***4. Refactor using Lens*** 

TODO
