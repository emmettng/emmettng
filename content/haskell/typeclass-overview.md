---
title: "Typeclass Overview"
date: 2019-12-24T10:58:12+08:00
draft: true
---

### Readings

- **basic**
  - [typeclass wiki](https://wiki.haskell.org/Typeclassopedia) : Worth reading every couple of weeks.
  - [mmhaskell: why haskell](https://mmhaskell.com/blog/2019/1/7/why-haskell-i-simple-data-types)

- **advanced**
  - [optic types](http://oleg.fi/gists/posts/2017-04-18-glassery.html)
  - [optic well typed](http://www.well-typed.com/blog/2019/09/announcing-the-optics-library/)

## Recap 
- Engineering point of view: haskell provides better factorization. 
`Better` refers to:
  1. The effect of local modification is predictable.
  1. Business requirement (Functionality) could be organized or analysed by manipulating meaningful type description.
- An application can be factorized into two parts:
  1. A ***Computational Chain*** that is also a type transformation that focus on `Target type`s from ` a -> b -> c -> d -> ... -> z`.
  2. This computational chain constantly switch among different ***Computational Contexts***, `T1` to `T2` to `T2 ...`

## Parametric Types
>**Every piece of information matters in its own way.**

1. `Type Constructor` defines `global representation` of a `type`.
2. `Type Constructor` may has 
    - ***Zero*** argument (`a`): Then the `global representation` carries all information of this type .
    - ***More*** arguments (`T a`): Then the `global representation` consists of 
      - `a` : can be treated as the `Target type` 
      - `T` : can be treated as the `Context type`
```
Example:

data Tree a = Tip | Node a (Tree a) (Tree a)
```
  - **Global Representation** (***Type constructor***): `Tree a`
  - **Local Representation** (***Value constructor***): ` Tip | Node a (Tree a) ( Tree a)`
  - ***Target Type*** : `'a'` in `global representation`.
  - ***Context type*** : ` 'Tree' ` in `global representation`.

Thanks to the [Currying](https://wiki.haskell.org/Currying) , `Type Constructor` could be parametrized. Usually, and in this doc, the computational chain (function composition) focus on the transform of `target types`. Each computation focus on **One target type at a time**. AND:

- ***Target Type*** : Carries information type we care about. So reasoning behaviour of the target computational chain would be easy.
- ***Context Type***: This extra piece of information is a wrapper of the target information `a`. It could be used to 
  1. Represent how target information being organized( data structure) 
  1. How target function will be affected in evaluation process(`Applicative`,`Monad`, ect details below)
  1. Wrap existing type for specific purpose. This context is also a global representation of this newly defined type.

-  Different ***Context Type*** indicates different data structure based on `a`. ( when **Target Type** be the same `a`)

    ```
    Example:
    
    data Tree a = Tip | Node a (Tree a) (Tree a)
    data List a = Nil | Cons a (List a)
    ```

- Being the instance of different typeclasses, one ***Context Type*** could carries different semantics, indicating how computational chain of the target type will be affected in certain way. (**This is what this doc all about**)
  
      [example](http://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Writer.Lazy.html#mapWriterT): 

  - `functor` + `Writer` ==> `Writer` is a container in this case.
  - `Monad` + `Writer` ==> `Writer` is a computational context relies on `Monoid` type to accumulate( `mappend`) some extra `logging` information `w`.

  ```
  mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
  mapWriterT f m = WriterT $ f (runWriterT m)

  instance (Functor m) => Functor (WriterT w m) where
    fmap f = mapWriterT $ fmap $ \ ~(a, w) -> (f a, w)
  
  instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = writer (a, mempty)
    m >>= k  = WriterT $ do
        ~(a, w)  <- runWriterT m
        ~(b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
  ```
- `Context Type` as a wrapper of existing type could be used for specific purpose and avoiding being ambiguous. 
  
      example:
 
      `(,)` and `Writer` are equal up to isomorphism.
      ```
          > type P = forall a. ((,)a)
          > :info P
          type P = forall a. (,) a :: * -> *
      ```
      ```
        newtype Writer m a = Writer {runWriter :: (a , w)}
      ```
      They contain equal amount of information. The `Writer` part in `newtype Writer` is being used to identify possible different instance of the same `typeclass`.

      example:

        ```
        instance Monoid a => Applicative ((,) a) where
          pure x = (mempty, x)
          (u, f) <*> (v, x) = (u <> v, f x)
          liftA2 f (u, x) (v, y) = (u <> v, f x y)

        instance (Monoid w) => Applicative (Writer w ) where
          pure a  = Writer $ (a, empty)
          f <*> v = Writer $ k f v
                where k (a, w) (b, w') = (a b, w `append` w')
        ```
      Both being instance of `Applicative` with same context semantics, `(,)` relies on the `first` element of being `Monoid` type, `Writer` relies on the `second` element of being a `Monoid` type. 

***Parametric types is the foundation*** of decomposing an application into `computational chain` and `computational context` factors.***

A functional application is just a recomposition of `computational chains` of different `computational context`.

Category theory and haskell guarantee that the behaviour of recomposing a long `computation chain` would be predicable. 

> **Predictable** does not implies good implementation. There are design principles and design patterns for functional programming that we need to follow for achieving scalability and maintainability. 

## Section 0. `Computational Context`


> **Parametric type** X **Typeclasses** = **Computational Context**

Three common `Typeclass`es, that being used to define how, a context type `f`, could affect the target computation `a -> b`.

| `typeclass`| `main function`| `function type`|
|:--:|:--:|:--:|
|`Functor`| `<$>`| `(a -> b) -> f a -> f b`|
|`Applicative`| `<*>` |`f(a -> b) -> f a -> f b`|
|`Monad`|`>>=` | `f a -> ( a -> f b) -> f b`|

Flip the argument of `>>=`, we get `(a-> f b) -> f a -> f b`. Now it is much clear. These function types could be seen as two parts. The first part includes:

1. ` (a -> b)`, `f (a -> b)`, ` a -> f b`

The second part of these three functions are all the same:

2. `f a - f b`

Intuitively, these three functions are about how to transform different mapping relations (in `1`) into context sensitive mapping `f a -> f b`. So that we could compose functions of the same `Context Semantics`.


When `f a` is a certain `Parametric Type`, it could indicate a specific `Computational Context` with a specific `Context Semantics`.

|**Parametric types**:|  List | Product    |Sum   |  Exponential |   
|:--|:--|:--:|:--:|:--:|:--:|
|`<$>`|  Container | Container |Container |Container |
|`<*>`|  Generator|Container |Container |Container |
|`>>=`|  `[]` | Context Writer | Context Either/Maybe/IO | Context Reader/State



***Summary***

`<*> :: f (a ->b) -> f a -> f b` Context information starts to effect

`<$>` cannot handle this

`>=> :: (a -> f b) -> (b -> fc) -> (a -> fc)`

`<*>` cannot provide this composition ability

`>>=` where the Context information starts to shine

example of `applicativeDo`
```
would normally be desugared to foo1 >>= \x -> foo2 >>= \y -> foo3 >>= \z -> return (g x y z), but this is equivalent to g <$> foo1 <*> foo2 <*> foo3. With the ApplicativeDo extension enabled (as of GHC 8.0), GHC tries hard to desugar do-blocks using Applicative operations wherever possible. This can sometimes lead to efficiency gains, even for types which also have Monad instances, since in general Applicative computations may be run in parallel, whereas monadic ones may not. For example, consider

g :: Int -> Int -> M Int

-- These could be expensive
bar, baz :: M Int

foo :: M Int
foo = do
  x <- bar
  y <- baz
  g x y
foo definitely depends on the Monad instance of M, since the effects generated by the whole computation may depend (via g) on the Int outputs of bar and baz. Nonetheless, with ApplicativeDo enabled, foo can be desugared as

join (g <$> bar <*> baz)
which may allow bar and baz to be computed in parallel, since they at least do not depend on each other.

The ApplicativeDo extension is described in this wiki page, and in more detail in this Haskell Symposium paper.
```

`Parametric types` + `Typeclass` = `Intuitive semantics`

- `Container` : Value/s of Type a ( deterministic)
- `Generator` : Value/s of Type a ( non-deterministic)
  - `f <*> xs`: Every element of the list `xs` will be input of function `f`.
  - Default semantics of applicative functor of `[]`.



#### MonadTrans

`T m a `

- Function type monad transfers `T` use `m` to wrap target type `a`
```
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
instance (Monad m) => Monad (StateT s m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = StateT $ \ s -> return (a, s)
    {-# INLINE return #-}
#endif
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}
    fail str = StateT $ \ _ -> fail str
    {-# INLINE fail #-}
```
- Parametric type monad transfers `T` reach inside `m` to wrap target type `a`
```
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    return = lift . return
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
```

> For example, when a `StateT s Maybe a` computation fails, the state ceases being updated (indeed, it simply disappears); on the other hand, the state of a `MaybeT (State s) a` computation may continue to be modified even after the computation has "failed". This may seem backwards, but it is correct. [[haskell wiki:typeclass](https://wiki.haskell.org/Typeclassopedia#Monad_transformers)]

**Common usage**
- When many `T m a` composed with `>>=`, the `Computational Context` is the outside `T`.
- `runT` unwrap this `monad stack`, thus switch the `Computational Context` from `T` to `m`.

## TODO:
- TODO: needs more examples about how this works, such as `MaybeT (State s) a`
  - More about MonadTrans typeclass and [Transformer type classes](https://wiki.haskell.org/Typeclassopedia#Transformer_type_classes_and_.22capability.22_style)
   ([mtl](http://hackage.haskell.org/package/mtl)). need isolated tutorial and examples set. `mtl` obsolete the need of `lift` in `Monad.Trans` (more details) typelcass wiki has alot great further reading in monad transfer part.
  - check corresponding `>>=` , `lift` and `liftIO` function for more details.
    ```
    the effects of inner monads "have precedence" over the effects of outer ones. 
    ```
    wirte function focus on innermost monad and `lift` all the way up to the outer most. `More Examples`
  - Summarize [RWST](http://hackage.haskell.org/package/transformers-0.2.2.0/docs/src/Control-Monad-Trans-RWS-Lazy.html)


## Section 1. Transform within the same type
`Semigroup` and `Monoid` represent  `type :: a` and `an operation :: a -> a -> a` on that `type`.

#### Readings
- [schoolofhaskell](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour)

### 1. Semigroup ***[Data.Semigroup](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html)***

Define binary function `<>` that take input and give output of the same type.

```
class Semigroup a where
  (<>) :: a -> a -> a

  default (<>) :: Monoid a => a -> a -> a
  (<>) = mappend
```
### 2. Monoid

Define `mempty :: a ` that enabling `(<> mempty)` to be an `identity function` on type `a`.

```
class Semigroup a => Monoid a where
        -- | Identity of 'mappend'
        mempty  :: a

        -- | An associative operation
        --
        -- __NOTE__: This method is redundant and has the default
        -- implementation @'mappend' = '(<>)'@ since /base-4.11.0.0/.
        mappend :: a -> a -> a
        mappend = (<>)
        {-# INLINE mappend #-}

        -- | Fold a list using the monoid.
        --
        -- For most types, the default definition for 'mconcat' will be
        -- used, but the function is included in the class definition so
        -- that an optimized version can be provided for specific types.
        mconcat :: [a] -> a
        mconcat = foldr mappend mempty
```
  - `Dual` [Data.Monoid](https://hackage.haskell.org/package/base-4.9.0.0/docs/src/Data.Monoid.html#Dual)
    ```
    package Data.Semigroup:
    instance Semigroup a => Semigroup (Dual a) where
      Dual a <> Dual b = Dual (b <> a)
      stimes n (Dual a) = Dual (stimes n a)

    package Data.Monoid:
    instance Monoid a => Monoid (Dual a) where
            mempty = Dual mempty
            Dual x `mappend` Dual y = Dual (y `mappend` x)
    ``` 
## Section 2: Monoidal subclass 
0. In a `monoidal operation :: t -> t -> t`, `t` could be a `parametric type`.


|**Typeclass**|  | List |product    |Sum   |  -> |
|:--:|:--:|:--:|:--:|:--:|:--:|
|`<|>`| `:: f a-> f a -> f a`| Container | Container |Container |Container |
|`mplus`:: m a-> m a -> m a`| Container | Container |Container |Container |

```
Maybe Example:
instance (Functor m, Monad m) => Alternative (MaybeT m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => MonadPlus (MaybeT m) where
    mzero = MaybeT (return Nothing)
    mplus x y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v
```

`Reader Example:`
```
instance (Alternative m) => Alternative (ReaderT r m) where
    empty   = liftReaderT empty
    {-# INLINE empty #-}
    m <|> n = ReaderT $ \ r -> runReaderT m r <|> runReaderT n r
    {-# INLINE (<|>) #-}

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = lift mzero
    {-# INLINE mzero #-}
    m `mplus` n = ReaderT $ \ r -> runReaderT m r `mplus` runReaderT n r
    {-# INLINE mplus #-}
```
`Writer Example:`
```
instance (Monoid w, Alternative m) => Alternative (WriterT w m) where
    empty   = WriterT empty
    {-# INLINE empty #-}
    m <|> n = WriterT $ runWriterT m <|> runWriterT n
    {-# INLINE (<|>) #-}

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero       = WriterT mzero
    {-# INLINE mzero #-}
    m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n
    {-# INLINE mplus #-}

``` 


Several classes have `monoidal subclass` to model computation that support `failure` or `choice`.

- Applicative 
  - Alternative
- Monad
    - MonadPlus
- Arrow
    - ArrowPlus
    - Arrow Zero



## Section 3: Utility Functions
#### 3.0.Content 
`:: (Functor t, Foldable t, Traversable t) => `  

| [`on monads `](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Control.Monad.html) | type `:: Monad m =>` | `on Applicative :: Applicative f` | type ` :: Applicative f =>`|
|:--:|:--:|:--:|:--:|  
|`return`| `a -> m a`|`pure`|`a -> f a`|    
|`liftM2`|`(a -> b -> c) -> m a -> m b -> m c`|`liftA2`|`( a -> b -> c) -> f a -> f b -> f c`|   
|`mapM`|`(a -> m b) -> t a -> m (t b)`|`traverse`|`(a -> f b) -> t a -> f (t b)`|  
|`forM`|`t a -> (a -> m b) -> m (t b)`|`for`| `t a -> (a -> f b) -> f (t b)`|  
|`sequence`|`t (m a) -> m ( t b)`|`sequenceA`|`t (f b) -> f ()`|
|`mapM_`|`(a -> m b) -> t a -> m ()`|`traverse`|`(a -> f b) -> t a -> f ()`|  
|`forM_`|`t a -> (a -> m b) -> m ()`|`for`| `t a -> (a -> f b) -> f ()`|
|`sequence_`|`t (m a) -> m ()`|`sequenceA`|`t (f b) -> f ()`|  

#### 3.1.Applicative and Monad 
|function| constraint|type| define | import |  
|:--:|:--:|:--:|:--:|:--:|:--:|
|[liftA2](https://stackoverflow.com/questions/47065766/how-does-the-default-definition-of-in-haskell-work)| `Applicative f =>` | `(a -> b -> c) -> (f a -> f b -> f c)`| `Control.Applicative` | `GHC.Base`|
|liftM2| `Monad m =>` | `(a -> b -> c) -> (m a -> m b -> m c)`| `Control.Monad` | `GHC.Base`|
- `liftA2 f x = <*> (fmap f x)`. Context information `f` affect function `a -> b -> c` in its own way. This default definition relies on `<*>` and `fmap`. Some support explicit definition which would be more efficient.
  - Sum type `Maybe` & `Either e` : All `f a`, `f b` and `f c` must be success or nothing but error `e`.
  - Product type `(,)` & `Writer w` : All logging information `w` be aggregated in the first element (in `(,)` ) or in the second element ( in `Writer` ).
  - Exponential type [`(->) e`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#Applicative) & [`Reader e`](http://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Reader.html#liftReaderT) : Now `f a` is of type  `e -> a`. Apply `f a`, `f b` with common environment information `e` respectively, then apply function `f :: a -> b -> c` on these two results.
- `liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2: return (f x2 x2)}`. This is the default definition and all `Parametric Types` above relies on this definition and has the same semantics as above.

#### 3.2.Foldable
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

#### 3.3.Traversable
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
 
#### 3.4. newtype
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
- Intuition and examples of these operations above.
- relation between `Monad` and `Applicative + Alternative`
  ```
  ∗ Actually, because Haskell allows general recursion, one can recursively construct infinite grammars, and hence Applicative (together with Alternative) is enough to parse any context-sensitive language with a finite alphabet. See Parsing context-sensitive languages with Applicative.
  ```
  How is this work ?
- finish `foldr` and `foldl`
-  more examples of traverse/mapM , foldMap, fold, foldM, sequenceA/sequence. Based on not only list, but also `Data.Tree`

## Section 4
- MonadTrans
- MonadIO
- MonadFail
- MonadFix (read after the first time finishing typeclass wiki)
- Contravarriant
    
    ```
    Prelude Control.Monad.Fail> :info fail
    class Monad m => MonadFail (m :: * -> *) where
    Control.Monad.Fail.fail :: String -> m a
    	-- Defined in ‘Control.Monad.Fail’

    class Applicative m => Monad (m :: * -> *) where
      ...
    Prelude.fail :: String -> m a
  	    -- Defined in ‘GHC.Base’

    ```
    TODO: Why MonadFail instead of Prelude.fail
    [readthis](https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail)


##TODO
- draw a relation graph of these useful operations above
- Intuition of these operations 
- relation between `Monad` and `Applicative + Alternative`
  ```
  ∗ Actually, because Haskell allows general recursion, one can recursively construct infinite grammars, and hence Applicative (together with Alternative) is enough to parse any context-sensitive language with a finite alphabet. See Parsing context-sensitive languages with Applicative.
  ```
  How is this work ?

 ## TODO 
  `newtype`
  [Mileski twitter example](https://twitter.com/BartoszMilewski/status/1210753654329790464)
  ```
  type ListPair a = [(Card,a)]
  instance Functor ListPair where 
    fmap f = fmap (bimap id f)
  
  > The type synonym 'ListPair' should have one argument but has been given one.
  > In the instance declaration for 'Functor ListPair'
  ```
  The error free form should be 
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