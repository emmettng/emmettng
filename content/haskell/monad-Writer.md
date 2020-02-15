---
title: "Monad: Writer"
date: 2019-11-06T11:15:38+08:00
draft: false
---
> This summary follows the minimum useable principle.

#### Readings
- [Yahtee1](http://h2.jaguarpaw.co.uk/posts/using-brain-less-refactoring-yahtzee/)
- [Yahtee2](http://h2.jaguarpaw.co.uk/posts/good-design-and-type-safety-in-yahtzee/)
- [haskell at work: domain modeling](https://haskell-at-work.com/)

```
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
```
**Simple definition** from *learn you a haskell*

```
newtype Writer w a = Writer { runWriter :: (a, w) }  

instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v') 
```

**Complete definition** from *Control.Monad.Trans*
```
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = writer (a, mempty)
    m >>= k  = WriterT $ do
        ~(a, w)  <- runWriterT m
        ~(b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
```

### Monadic Semantics
- Target type : `a`
- Context type : `(Monoid m) => Writer m` or `(Monoid m) => WriterT w m`
    - **Explicitly** : transformation among `target types :: a -> b -> c`.
    - **Implicitly** : Aggregate `logging` information of type `w` during the target types transformation.
    
> - There is a type `b`.
> - There is a function (`:: a -> b`) from `a` to `b`.
> - Many functions ` a -> b` , `b -> c` ... `y -> z` many compose as a transformation from `a -> z`.
> - Each function many produce some extra `logging` information of type `w`. 
> - The product of two types `w` and `b` is `(b,w)`.
> - We want the result of this transformation `a -> z`, we also want to aggregate the `logging` information of each function in this transformation.
> - We define a new type `newtype Writer w a = Writer {runWriter :: (a,w)}`
> - In `Writer Monad` we care about computation compositions :
>> - `>>= :: Writer w a -> (a -> Writer w b) -> Writer w b`.
>> - `>=> :: (a -> Writer w b) -> ( b -> Writer w c) -> (a -> Writer w c)`
> - The transformation of `target types` and `logging` information are being processed explicitly and implicitly respectively.

- Auxiliary functions `tell`,`return`, `listen(s)`, `pass`,`censor`
- These functions output Writer Monad.
- These special purpose Writer Monads are composed by `>>` operator usually.
- They collectively work as a single Writer Monad for certain function.


### wrtier
- `lift` and `wrap` a type product `(a,w)` to be a `WriterT` monad.
    ```
    writer :: (Monad m) => (a, w) -> WriterT w m a
    writer = WriterT . return
    ```
### tell
- Introduce the logging information `w`
    ```
    tell :: (Monad m) => w -> WriterT w m ()
    tell w = writer ((), w)
    ```

### return 
- Introduce the value of `target type: a`
    ```
    instance (Monoid w, Monad m) => Monad (WriterT w m) where
        return a = writer (a, mempty)
    ```
    Usually, `tell` and `return` are being used together in a `Writer Monad`:   
    >`.... tell w >> return a`
    `tell w` introduce the logging information and `return a` introduce the `target information`, `>>` combines these two together as a new `Writer Monad`.

### listen 
- Retrieve logging information `w` by transforming `target type` `a` to `(a,w)`. So that we could process `logging information w` explicitly.
    ```
    listen :: (Monad m) => WriterT w m a -> WriterT w m (a, w)
    listen m = WriterT $ do
        ~(a, w) <- runWriterT m
        return ((a, w), w)
    ```
### listens
- Retrieve logging information `w` from `Writer Monad` just like what `listen` does. Only apply a function of type `w -> b` on the `logging information.
    ```
    listens :: (Monad m) => (w -> b) -> WriterT w m a -> WriterT w m (a, b)
    listens f m = WriterT $ do
        ~(a, w) <- runWriterT m
        return ((a, f w), w)
    ```
### censor
- Transform existed `w` in `Writer Monad` with a function of type `w -> w`.
    ```
    censor :: (Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
    censor f m = WriterT $ do
        ~(a, w) <- runWriterT m
        return (a, f w)
    ```
### pass
- If our target type contains a function of type `w -> w` in this form `(a, w->w)`. `pass` preserve `a` in target type and apply the function `w -> w` in target type to the logging information `w` in context type.
    ```
    pass :: (Monad m) => WriterT w m (a, w -> w) -> WriterT w m a
    pass m = WriterT $ do
        ~((a, f), w) <- runWriterT m
        return (a, f w)
    ```
####Section Summary 
>Haskell enable us to decompose an application into `target computation` and `computation context`. So that we could manage to get predictable outcome by recomposing various `target computation` with `computation context`. These auxiliary function mainly about manipulating `Context` related information.


### Example 1: Simple Example

necessary imports:
```
import Control.Monad.Trans.Writer
import GHC.Float
```

Code:
```
f1 :: Int -> Writer String Float
f1 i = do
  tell $ show i
  return $ fromIntegral i + 0.2

f2 :: Float -> Writer String Double
f2 f = do
  tell $ show f
  return $ float2Double f * 2
```
Check in ghci:
```
> :info f1
f1 :: Int -> Writer String Float
> :info f2
f2 :: Float -> Writer String Double
> import Control.Monad    -- for the ( >=> ) operator
> let ff = f1 >=>f2
> :info ff
ff :: Int -> WriterT String Data.Functor.Identity.Identity Double
> let r = runWriter $ ff 10
> :info r
r :: (Double, String) 	-- Defined at <interactive>:20:5
> r
(20.399999618530273,"1010.2")
```
- `Double :: (10+0.2) * 2` 
- ```String :: "10" ++ "10.2" = "1010.2"```. Because `String` is an instance of `Monoid` and `Semigroup` the `<>` defined on `String` is `++`.


### Example 2: Real World Simple Example
necessary import:
```
import Data.Traversable
```
Code:
```
data LoggingType = LoggingType
  {
    partOne :: Int
  , partTwo :: Int
  }deriving (Show,Eq)

instance Semigroup LoggingType where
  (LoggingType o1 t1) <> (LoggingType o2 t2) = LoggingType (o1 + o2) (t1 + t2)

instance Monoid LoggingType where
  mempty = LoggingType 0 0

p1list = [1..10]
p2list = [2,4..40]

logList = uncurry LoggingType <$> zip p1list p2list

createLog :: (Int,Int) -> Writer LoggingType Int
createLog (e1, e2) =
  let w = LoggingType e1 e2
      s = e1 + e2
  in writer (s,w)

totalLog :: Writer LoggingType [Int]
totalLog = mapM createLog $ zip p1list p2list
```
Check in ghci:
```
> totalLog 
WriterT (Identity ([3,6,9,12,15,18,21,24,27,30],LoggingType {partOne = 55, partTwo = 110}))
```

Intuition:

- Target Operation:
    - `p1list` and `p2list` zip together produces the target input of type `[(Int,Int)]`.
    - The target transform is `(Int,Int) -> Int`. Which is `s = e1 + e2`.
    - Usually we use `map` or `fmap (<$>)` to lift this transform so it works in the `[]` container.
        ```
        [(1,2),(2,4),(3,6)...(10,20)] -> [3,5,9,...,30]
        fmap (Int,Int) -> Int   [...]
        ```
- Context Semantics: 
    - We want to log some information for each `target transform` of the element.
    - Accumulate the sum of the first list and second list respectively.
    - So define `LoggingType` as the instance of `Semigroup` and `Monoid`.
    - The logging information is `LoggingType e1 e2`.
        ```
        [(1,2),(2,4),(3,6)...(10,20)] -> [3,5,9,...,30]
        mapM (Int,Int) -> Writer LoggingType Int   [...]
        ```
- This newly Context sensitive transform `createLog` need to cooperate with `mapM` instead of `map`.
- The `Target Computation` and `Computation Context` would more clear if we rewrite `createLog` as:
     ```
     createLog (e1,e2) = do
       tell $ LoggingType e1 e2      -- tell : Context Operation :: LoggingType
       pure $ e1 + e2                -- pure : Target Operation :: Int
     ```
- `pure` is always about bring value of `target type` into this `Computation Context`
