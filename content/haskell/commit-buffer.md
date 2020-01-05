---
title: "Commit Buffer"
date: 2020-01-03T10:19:51+08:00
draft: true
---

### 0.Content 
`:: (Functor t, Foldable t, Traversable t) => `
| [`on monads `](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Control.Monad.html) | type `:: Monad m =>` | `on Applicative :: Applicative f` | type ` :: Applicative f =>`|
|--:|:--|--:|:--|
|`return`| `a -m a`|`pure`|`a -> f a`|
|`liftM2`|`(a -> b -> c) -> m a -> m b -> m c`|`liftA2`|`( a -> b -> c) -> f a -> f b -> f c`|
|`mapM`|`(a -> m b) -> t a -> m (t b)`|`traverse`|`(a -> f b) -> t a -> f (t b)`|
|`forM`|`t a -> (a -> m b) -> m (t b)`|`for`| `t a -> (a -> f b) -> f (t b)`|
|`sequence`|`t (m a) -> m ( t b)`|`sequenceA`|`t (f b) -> f ()`|
|`mapM_`|`(a -> m b) -> t a -> m ()`|`traverse`|`(a -> f b) -> t a -> f ()`|
|`forM_`|`t a -> (a -> m b) -> m ()`|`for`| `t a -> (a -> f b) -> f ()`|
|`sequence_`|`t (m a) -> m ()`|`sequenceA`|`t (f b) -> f ()`|
### 1.Applicative and Monad
|function| constraint|type| define | import |
|:--:|:--:|:--:|:--:|:--:|:--:|
|[liftA2](https://stackoverflow.com/questions/47065766/how-does-the-default-definition-of-in-haskell-work)| `Applicative f =>` | `(a -> b -> c) -> (f a -> f b -> f c)`| `Control.Applicative` | `GHC.Base`|
|liftM2| `Monad m =>` | `(a -> b -> c) -> (m a -> m b -> m c)`| `Control.Monad` | `GHC.Base`|
- `liftA2 f x = <*> (fmap f x)`. Context information `f` affect function `a -> b -> c` in its own way. This default definition relies on `<*>` and `fmap`. Some support explicit definition which would be more efficient.
  - Sum type `Maybe` & `Either e` : All `f a`, `f b` and `f c` must be success or nothing but error `e`.
  - Product type `(,)` & `Writer w` : All logging information `w` be aggregated in the first element (in `(,)` ) or in the second element ( in `Writer` ).
  - Exponential type [`(->) e`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#Applicative) & [`Reader e`](http://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Reader.html#liftReaderT) : Now `f a` is of type  `e -> a`. Apply `f a`, `f b` with common environment information `e` respectively, then apply function `f :: a -> b -> c` on these two results.
- `liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2: return (f x2 x2)}`. This is the default definition and all `Parametric Types` above relies on this definition and has the same semantics as above.

### 2.Foldable
|function| constraint|type| define | import |
|:--:|:--:|:--:|:--:|:--:|:--:|
|`foldMap`| `Monoid m, Foldable t`| `(a -> m) -> t a -> m`| `Data.Foldable` | `GHC.Base`|
|`fold`| `Monoid m, Foldable t`| `t a -> m`| `Data.Foldable` | `Data.Foldable`|
|`foldrM`| `Monad m, Foldable t`| `(a -> b -> m b) -> b -> t a -> m b`| `Data.Foldable` | `Data.Foldable`|
|`foldlM` = `foldM`| `Monad m, Foldable t`| `(b -> a -> m b) -> b -> t a -> m b`| `Data.Foldable` | `Data.Foldable`|

- `foldMap` substitute target type `a` in `foldable` context with `Monoid` type `m`, then collapse `Foldable m` to a single value of type `m`.
    ```
        foldMap :: Monoid m => (a -> m) -> t a -> m
        foldMap f = foldr (mappend . f) mempty
    ```
    - `fold = foldMap id`
- `foldrM` and `foldlM (foldM)` focus on folding over monadic values using `>>=`. Detailed discussion [here](https://stackoverflow.com/questions/58443912/what-kind-of-knowledge-or-training-is-necessary-for-someone-to-write-down-the-de)
    ```
    -- | Monadic fold over the elements of a structure,
    -- associating to the right, i.e. from right to left.
    foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
    foldrM f z0 xs = foldl f' return xs z0
      where f' k x z = f x z >>= k

    -- | Monadic fold over the elements of a structure,
    -- associating to the left, i.e. from left to right.
    foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
    foldlM f z0 xs = foldr f' return xs z0
      where f' x k z = f z x >>= k
    ```
    One possible example is:
    - `b -> a -> m b` : is a function that test whether `b` satisfied condition `a`. The test result is `m b` which contains both success and failure information.
    - `b` is the initial input being tested.
    - `t a` could be a list of condition `a`
    - `m b` is the final collapsed result of a sequence of test on `t a`.

## 3.Traversable
|function| constraint|type| define | import |
|:--:|:--:|:--:|:--:|:--:|
|`traverse`| `Applicative f`, `(Functor, Foldable, Traversable t)`| `(a -> f b) -> t b -> f (t b)`| `Data.Traversable`|`Prelude`|
|`mapM`| `Monad m`, `(Functor, Foldable, Traversable t)`| `(a -> m b) -> m b -> m (t b)`| `Data.Traversable`|`Prelude`|
- The [default](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Traversable.html#mapM) definition is: `mapM = traverse`.
- The relation between `traverse` and `sequenceA` kind like the relation between `liftA2` and `<*>`. Here is the default definition:
  ```
  class (Functor t, Foldable t) => Traversable (t :: * -> *) where
    ...
    traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f
    ...
    sequenceA :: (Applicative f) => t (f b) -> f ( t b)
    sequenceA = traverse id
  ```
  In general, it is better to write traverse when implementing Traversable, as the default definition of `traverse` performs, in principle, two runs across the structure (one for fmap and another for sequenceA).
- `traverse` defines how `Target computation :: a -> b` works on target type `a` in a `Foldable` structure `t`. Meanwhile Context information `f` affect the ultimate result: ` f ( t b)`.
- Generally, default definition: `mapM = traverse`. in [GHC.Base](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#mapM) `mapM` is specifically defined for `Traversable` list:
  ```
  mapM :: Monad m => (a -> m b) -> [a] -> m [b]
  mapM f as = foldr k (return []) as
              where
                k a r = do { x <- f a; xs <- r; return (x:xs) }
  ```
|function| constraint|type| define | import |
|:--:|:--:|:--:|:--:|:--:|
|`for`|`Applicative f`, `(Functor, Foldable, Traversable t)`| ` t b -> (a -> f b)-> f (t b)`| `Data.Traversable`|`Data.Traversable`|
|`forM`| `Monad m`, `(Functor, Foldable, Traversable t)`| ` m b -> (a -> m b)-> m (t b)`| `Data.Traversable`|`Prelude`|
- `for` is 'traverse' with its arguments flipped. 
- `forM` is 'mapM' with its arguments flipped. 
 
 
|function| constraint|type| define | import |
|:--:|:--:|:--:|:--:|:--:|
|`sequenceA`| `Applicative f`, `(Functor, Foldable, Traversable t)`| `t (f a) -> f (t a)`| `Data.Traversable`|`Prelude`|
|`sequence`| `Monad m`, `(Functor, Foldable, Traversable t)`| ` t (m a) -> m (t a)`| `Data.Traversable`|`GHC.Base`|
- TODO: more intuitive examples

> “underscored” variants, such as sequence_ and mapM_; these variants throw away the results of the computations passed to them as arguments, using them only for their side effects.
>
> | [`monad constrained`](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Control.Monad.html) | type | define | import|
> |:--:|:--:|:--:|:--:|
> |`mapM_`|`(Foldable t, Monad m) => (a -> m b) -> t a -> m ()`| `Data.Foldable`| `Prelude`|
> |`forM_`|`(Foldable t, Monad m) => t a -> (a -> m b) -> m ()`|`Data.Foldable`|`Data.Foldable`|
> |`sequence_`|`(Foldable t, Monad m) => t (m a) -> m ()`| `Data.Foldable`| `Prelude`|


> | `Applicative constrained` | type|define | import|
> |:--:|:--:|:--:|:--:|
> |`traverse_`| `(Foldable t, Applicative f) => (a -> f b) -> t a -> f ()`|`Data.Foldable` |`Data.Foldable`|
> |`for_`|`(Foldable t, Applicative f) => t a -> (a -> f b) -> f ()`|`Data.Foldable`|`Data.Foldable`|
> |`sequenceA_`|`(Foldable t, Applicative f) => t (f a) -> f ()`| `Data.Foldable`| `Data.Foldable`|
>
>- This so call `side effects` is the what these functions all about.
>- All these functions are defined in `Data.Foldable`.
>- `mapM_` and `sequence_` are exposed by `Prelude`.
>- TODO: needs more attention and example.
 
### 4. newtype
  [Mileski twitter example](https://twitter.com/BartoszMilewski/status/1210753654329790464)
  ```
  type ListPair a = [(Card,a)]
  instance Functor ListPair where 
    fmap f = fmap (bimap id f)
  
  > The type synonym 'ListPair' should have one argument but has been given one.
  > In the instance declaration for 'Functor ListPair'
  ```
  The correct definition should be :
  ```
  newtype ListPair a = LP {unLP :: [(Card,a)]}
  instance Functor ListPair where 
    fmap f = fmap (bimap id f)
  ```
  Then wrap and unwrap value of type `ListPair` explicitly.

  - If the function instance declaration with `type` synonym works.
    ```
    t :: [(Card,String)]
    h :: (Card,String) -> a
    g :: String -> a

    let fh = fmap h t     -- function fh
    let fg = fmap g t     -- function fg
    ```
    the `fmap` in function `fh` is defined in `instance Functor []`
    the `fmap` in function `fg` is defined in `instance Functor ListPair`
    In this case what instance will being used is not determined by the type of `t`, it is also determined by the type of `f` (as in `fmap f ` in this example `fg` and `fh`). This can be achieved, however:
      - most of the time the instance of fmap being used can be determined by the type of `t` only. Checking two palces `t` and `f` is no efficient. 
      - It is simple to introduce a `newtype` as the wrapper to unify the place that contains information about instance will being used. ( in `t` only) 

## TODO
- draw a relation graph of these useful operations above
- Intuition of these operations 
- relation between `Monad` and `Applicative + Alternative`
  ```
  ∗ Actually, because Haskell allows general recursion, one can recursively construct infinite grammars, and hence Applicative (together with Alternative) is enough to parse any context-sensitive language with a finite alphabet. See Parsing context-sensitive languages with Applicative.
  ```
  How is this work ?
- finish `foldr` and `foldl`
-  more examples of traverse/mapM , foldMap, fold, foldM, sequenceA/sequence. Based on not only list, but also `Data.Tree`