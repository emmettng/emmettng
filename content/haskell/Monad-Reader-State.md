---
title: "Monad: Reader & State"
date: 2019-11-18T00:18:29+08:00
draft: true
---
> This document follows the minimum useable principle.

# Reader

#### online resources

- This one 
- [hackage: transformers ](http://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Reader.html)

## summary 
> Start from Examples below for quick recap.

**terms** 

- function chain:        
  A sequence of functions that compose.    
  - `A :: a -> b`
  - `B:: b -> c`
  - `C :: c -> d`
  - `D:: d-> e`

    A chain of functions above:         
    ` D . C . B . A :: a -> e`
  
**intuitation recap**

   - pass enviroment information `env` through all components of a function chain. 
   - Common usage include:  
  
        > ask  :: introducing `env` into function chain.    
        > local :: temporarily change `env` value or type.     
        > asks :: swiftly turn a function of type `env -> result` into this moand.    
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
```


### 1.Example one 

- A chain of Core functions.
- Intuition:
    - a Core input -> a chain of functions which inputs and outputs aligned and represented by a Reader Monad.
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
    - Input is from a1 of type `Int`
    - chain output is from a3 of type `Double`
    - Only when outinput and `env` being provided, the result can be produced.
    ```
    chainA :: Int -> Reader Int Double
    chainA n = do
        t1 <- a1 n          -- core operaiton 1
        t2 <- a2 t1         -- core operaiton 2
        a3 t2               -- core operation 3
    ```
    **alternative chain method:** `kleisli arrow`
    ```
    chainA' :: Int -> Reader Int Double
    chainA' = a1 >=> a2 >=> a3
    ```
***ask***
```
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return 

return :: r -> m r
```

***example output***
```
> let p1 = chainA 10
> runReader p1 $ 2
> 0.3333333432674408
```
In this example `env` is 2 and `n`(core input) is 10 so :
```
t1 = 10 + 2 , t2 = 2 / 12,  t3 = 2*2 / 12  =  0.33333333
```
### 2.Example Two
- Functions depend only on Env information.
- Intuition :
    - Sequence opeartion depends only on Enviroment. 


```
b1 :: (MonadIO m) => ReaderT String m Int
b1 = do
    env <- ask           -- get Enviroment information
    n   <- liftIO getLine
    let n1 = read env
        n2 = read n
    return $ n1 + n2
```
```
b2 :: (MonadIO m) => ReaderT String m Float
b2 = do
    env <- ask          -- get Enviroment information
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
asks :: (Monad m) => (r - a) -> ReaderT r m a 
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
