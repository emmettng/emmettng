---
title: "Monad: State"
date: 2019-12-02T14:19:49+08:00
draft: false 
---
> This summary follows the minimum useable principle.

#### Path 
- [learn you a haskell: for a few monads and more](http://learnyouahaskell.com/for-a-few-monads-more)
- [Hackage Control.Monad.Trans.State.Lazy](http://hackage.haskell.org/package/transformers-0.5.4.0/docs/src/Control-Monad-Trans-State-Lazy.html#StateT)

``` 
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
```

**Simple definition** from *learn you a haskell*
```
newtype State s a = State { runState :: s -> (a,s) }  
```
{{< image src="/imgs/lyah_state.png" alt="State Monad" position="center" style="border-radius: 8px;" >}}


**Complete definition** from *Control.Monad.Trans*
```
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \ s -> return (a, s)
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \ _ -> fail str
```

**Self recap** 
{{< image src="/imgs/state_recap.jpg" alt="State Monad" position="center" style="border-radius: 8px;" >}}



### Monadic Semantics 
> - A piece of information `s` affects the output of the computation `a -> b`.
>> `s -> a -> b == a -> s -> b`
> - Sometimes, the value of `s` will be updated and could affect following computation, so need to be preserved.
>>  `a -> s -> (b,s)` 
> - Two or more computations like this composed by `>>=` or `>=>` means each computation use the information `s` passed by previous one.
>> ` f >>= g :: s0 -> (a,s1) -> ( a -> s1 -> (b,s2)) -> (s0 -> (b,s2)` \
>> `s0, s1, s2` are values of the type `s`.   
>> Clearly, `>>=` and `>=>` chained these computations and hide the intermediate state `s1`, produce a function between `s0` and `s2`.
- auxiliary functions `get`, `put`, `return`
- each produce a State monad in different purposes
- these special purpose State monads are composed by `>>=` operator.
- They collectively work as a single State Monad for certain function.

### get
- `get` the current value of the state from previous State computation.
```
get :: (Monad m) => StateT s m s
get = state $ \ s -> (s, s)
```

### put
- `put` updates the value of `s` .
```
put :: (Monad m) => s -> StateT s m ()
put s = state $ \ _ -> ((), s)
```

### return
- combine the computation result `a` together with whatever `s` is.
```
instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \ s -> return (a, s)
```
### `>>=`
```
 m >>= k  = StateT $ \ s -> do
     ~(a, s') <- runStateT m s
     runStateT (k a) s'
```
#### summary 
Use `get` to receive `s` from previous State computation, use `put` to update the value of `s` if necessary, and combine computation result `a` with `s`.

### state
- turn a simple state style computation into State Monad
```
state :: (Monad m)
      => (s -> (a, s))  -- ^pure state transformer
      -> StateT s m a   -- ^equivalent state-passing computation
state f = StateT (return . f) -- this return is pair with m above.
```

### runState
- Unwrap a state monad computation as a function.( The inverse of 'state'.)
```
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
runState :: State s a   -- ^state-passing computation to execute
         -> s           -- ^initial state
         -> (a, s)      -- ^return value and final state
runState m = runIdentity . runStateT m
```

### evalStateT
- When we do not need the information in `s` anymore. 
- Use `evalStateT` or `evalState` to get function of type `s -> m a` or `s -> a`
```
evalStateT
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    ~(a, _) <- runStateT m s
    return a
```
```
evalState :: State s a  -- ^state-passing computation to execute
          -> s          -- ^initial value
          -> a          -- ^return value of the state computation
evalState m s = fst (runState m s)
{-# INLINE evalState #-}
```

#### Common usage
1. use `get` to introduce state. (compulsory)
2. some pure function :: `s -> a` will work on s . (optional)
3. `return` wrap `a` into State Monad. (compulsory)
4. `put` updates the state. (optional)
5. `evaState` / `evalStateT`    
             or   
    `runState` / `runStateT`    
  get the function of type ` :: s -> (a,s) `wrapped inside. They each works slight differently. (compulsory)    
    5.1 runState / runStateT retrive function ` s-> (a,s)` or `s -> m (a,s)`    
    5.2 evalState / evalStateT retrive function ` s -> a` or ` s -> m a`
6. feed to initial state `s0` into the function `s -> (a,s)` and get the final result `a` and final state `s`.

#### Intuition:
1. State Monad wrap a function from type `s` to an core output `a` and new value of type `s`.
2. `get`, `put`, `return` each represent Reader Monad of different specific purpose.
3. Usually, they composed (>>=) (>>) together to form a functional State Monad.

#### necessary import 
```
-- | Imports before Example One

import Control.Monad.Trans.State
import Control.Monad                -- for operator >=>
```
### Example one

- In general . State Monad is a function, the output include an extra part of information that is of the same type of the input.
- The input of `>=> or >>=` operation will always be the first one
- the Initial state will also be the first one as well .
```
s1 :: Int -> State String Float
s1 i = do
  s <- get
  let
    is = show i ++ "_"++ s
    r = fromIntegral i / (fromIntegral . length) is
  put is
  return r
```

```
s2 :: Float -> State String String
s2 f = do
  s <- get
  let
    sf = show f ++ "_"++ s
  put sf
  return sf
```

```
s3 :: String -> State String Int
s3 str = do
  s <- get
  let
    ns = str ++ "_" ++ s
  put ns
  return $ length ns
```

```
sChainOne :: Int -> State String Int
sChainOne = s1 >=> s2 >=> s3
```

```
> :info sChainOne
sChainOne :: Int -> State String Int
> let sFunc = runState $ sChainOne 20
sFunc :: String -> (Int, String) 	
> let r = sFunc "emmettng"
> r
(43,"1.8181819_20_emmettng_1.8181819_20_emmettng")
```