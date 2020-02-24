---
title: "Typeclass: Foldable"
date: 2020-01-06T00:34:46+08:00
draft: false
---
# Foldable

### Readings
- [Haskell wikibook: Foldable](https://en.wikibooks.org/wiki/Haskell/Foldable)
- [Youtube ConfEngine](https://www.youtube.com/watch?v=t9pxo7L8mS0)

## Parametric Type : `t a`
- Target type : `a`
- Context type : `t`
  
## Intuition based on Algebraic Data Type
> This section needs more theoretical backup.\
> Jump to `Intuition for Real World Implementation` for recap.

| instance declaration| `Context type` | define |
|:--:|:--:|:--:|
|`instance Foldable`|`Tree`|in ‘Data.Tree’|
|`instance Foldable`|`[]`|in ‘Data.Foldable’|
|`instance Foldable`|`Maybe`|in ‘Data.Foldable’|
|`instance Foldable`|`(Either a)`|in ‘Data.Foldable’|
|`instance Foldable`|`((,) a)`|in ‘Data.Foldable’|

### It's all about `foldMap`

- When parametric type `t a` represents some data structure. One or more value of target type `a` could be contained in this data structure `t a`.
- `foldMap` is about two operations:
  - ***a***. Replace `Target Type a` with a `Monoidal` type `m`.
  - ***b***. Aggregate values of type `m` with `<>`, if there are many values of `m`.
    ```
    typeClass Foldable  t where 
        ...
        foldMap :: Monoid m => (a -> m) -> t a -> m
        ...
    ```

**1.Only one value of Target Type** `a` **in** `t a`.


- Because there is only one value of type `a`:
   - Step ***b*** is not necessary in this case.
   - Just guarantee replacing the `target type a` with a `monoidal type m` and other information with `mempty` ( in `Sum type :: | `) or simply being ignored ( in `Product type :: (,)`).
 
- `Sum Type`: 
    - [`Maybe`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html):
       ```
       instance Foldable Maybe where
           foldMap = maybe mempty
           ...

       maybe :: b -> (a -> b) -> Maybe a -> b 	-- Defined in ‘Data.Maybe’
       ```
    - [`Either a`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html):
       ```
       instance Foldable (Either a) where
           foldMap _ (Left _) = mempty
           foldMap f (Right y) = f y
           ...
       ```
- `Product Type`:
    - [`(,) a`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html):
       ```
       instance Foldable ((,) a) where
           foldMap f (_, y) = f y
           ...
       ```


**2.More values of `Target type`** `a` **in** `t a`.

> **This is the main part of this doc.**

 If there is more than one value of the `target type` in the data structure, they must be organized based on the `Product` of `Target type a`. Two most used examples: 

   - `List`
       ```
       List a == f a  = 1 + a * f a
                      = 1 + a * (1 + a * (1 + a * (1 + a*(1 + a * ...))))
                      = 1 + a + a*a + a*a*a + ... + a*a*.... 
       ```
   - `Tree`
       ```
        Equation One: List a == f a = 1 + a * f a
        Equation Two: Tree a == T a = a * List (T a)
       ```
        Replace `a` in  with `T a` in `Equation One` and put it back to `Equation Two`, then:
       ``` 
        Tree a == T a = a * List (T a)
                      = a * (1 + (T a) + (T a)^2 + ... + (T a)^n)
                      = a + a * (T a) + a * (T a)^2 + ...+ a * (T a)^n
       ```

So we know:

   - Type `List a`: `1` means a data structure contains No information of `a`. `a` means one value of type `a`. `a*a` means two values of type `a`, etc.
       - `+` means `or`.
       - `a * a` equivalent to cartesian product of set `a` and `a`.
   - Type `Tree a`: at least one  piece of information of type `a`, or `a` and possible one or more trees `(T a)^n`.
       - Evaluate the definition of `Tree a` till there is no `T a` left, we can see that `Tree a = a + a*a + a*a*a + ... + a*a*...`.
       - The expansion of `Tree a` is quite similar with the expansion of `List a = 1+a+a*a+...`.

 2. Step `a` of `foldMap` replace `a` with `m`, we have:
       `List a = 1 + a + ... + a*a*... -> 1 + m + ... + m*m*...`
       `Tree a = a + a*a + ... + a*a*... -> m + m*m + ... + m*m*...`
 3. Step ***b*** of `foldMap`.
       A product `m*m*m` needs to be
       1. **Aggregated with `'<>'`**. 
       1. **Aggregated with `'<>'`**. 
       1. **Aggregated with `'<>'`**. \

       means replace `*` of `product type` with `<>`. \
   Now:\
   `(Monoid m) => m * m * m` == `m <> m <> m`.
 
>The computation `foldMap f List` are essentially the same as `foldMap f Tree`, except:
>
>1. When `List` could be empty, then: `foldMap f List = mempty`.
>2. `T` cannot be empty, it must contains at least one value of type `a`, it is equivalent to a List of only one element. In this case `foldMap f L = foldMap f T = m` 

### Summary
- `List` and `Tree` as instances of `Foldable` are equivalent up to the definition of `foldMap` (except for foldMap over empty list). More generally speaking, all combination of `Sum type` and `Product Type` are essentially the same as instances of `Foldable`.
- This make sense, because a `typeclass` reflects only one particular aspect of a given type. In other words, it defines some common property of many types which may contains more information but could be ignored when being treated as a `instance` of this `typeclass`.
- `FoldMap` does not utilize structure information of the `Parametric Type`. It simply:
  1. replace target type `a` with a monoid type `m`.
  3. When there is only one value of `a`, replace `a` with `m` and others with `memtpy`.
  2. When there are more values of `a`, replace `*` with `<>`.
   (***Whatever data structure it is***).

- **foldr**
  ```
  foldMap :: (Monoid m) => (a -> m) -> t a -> m 
  ```
  replace `m` with `b -> b`, we have
  ```
  aha :: (a -> b -> b) -> t a -> b -> b
  ```

  Function type `b -> b` is defined as an instance of [Monoid](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Semigroup.Internal.html#Endo) in Data.Semigroup.Internal. Wrapped by newtype `Endo`.


  So wrap ` b -> b` with `Endo` newtype and rearrange type signature of `aha` we have:
  ```
  class Foldable (t :: * -> *) where
    ...
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo . f) t) z
    ...
  ```

  1. `appEndo (foldMap (Endo . f) t)` 
      1. Replace ever `a` in `t` with a wrapped function `b -> b`
      1. Compose these functions together to have one function of type `b -> b`.
            ```
            newtype Endo a = Endo {appEndo :: a -> a}
            ```
      1. The type of function composition is
            ```
            Prelude GHC.Base Control.Monad> :info (.)
            (.) :: (b -> c) -> (a -> b) -> a -> c 	-- Defined in ‘GHC.Base’
            ```
            This means the first element of `t a` that starts affect the value of `b` is the right most element, then the one next to it. This is what the `r` in `foldr` means. 

  2. Then apply this function of type `b -> b` on `z` to get the final output `b`.
  3. Because a parametric type `t a` can be rewrite into `a*a*a*a` when being treated as an instance of `Foldable`. So a list is just:
     - ``` foldr (:) [] [1,2,3,4] ```
     - `[1,2,3,4]`, `(1,2,3,4)` contains the same amount of information.
     - Replace as described above will get ``` (1:(2:(3:(4:[])))) ```. It could also be written as ```(Con 1 (Con 2 (Con 3 (Con 4 []))))```
     - `[]` is the `List` type terminator of type `[]`.
  4. [`foldr`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldr) can be used to define [`foldMap`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldMap) as well.
        ```
        foldMap :: Monoid m => (a -> m) -> t a -> m
        foldMap f = foldr (mappend . f) mempty

        foldr :: (a -> b -> b) -> b -> t a -> b
        foldr f z t = appEndo (foldMap (Endo #. f) t) z
        ```

        ```
        foldr definition for List as the instance of Foldable:

        foldr k z = go
              where
                go []     = z
                go (y:ys) = y `k` go ys
        ```
## Intuition for Real World Implementation


1. **`foldr`** [intuition for `List`](https://www.youtube.com/watch?v=t9pxo7L8mS0)
    - Replace `Con` with `f`.
        - Or replace `:` with `f` (if `f` is an infix function).
    - Replace `Nil` with `z`.
    {{< image src="/imgs/foldr.jpg" alt="foldr" position="center" style="border-radius: 8px;" >}}
    ![foldr](../imgs/foldr.png)
    ```
    l = (Con a (Con b (Con c(Con ... (Con Nil))))
    foldr f z l =(f a (f b (f c(f ... (f z)))) 
    ```
    
    - ***Justificatoin***
      - `foldr (a -> b -> b) b (List a) = foldMap (a -> b -> b) (List a) $ b`
        replace `a` with `b -> b` , replace `*` with `<>` in this case `.` 
      - `foldr f z t = (b -> b) . (b -> b) . (b -> b) $ z`
      - ` Con :: a -> List -> List`\
        ` z = Nil`\
        `List a = (Con a (Con a (Con a..(Con a Nil))))`

      - `Con a (Con a( ....))` is the composition order, therefore
      - if ` f:: a -> b -> b` , replace `Con` with `f` and `Nil` with `z`.
      - `foldr f z l =(f a (f b (f c(f ... (f z))))`
   
2. **`foldr`** intuition for `Tree`
   - `Tree a` and `List a` are equivalent to each other as instances of `Foldable`.
   - Therefore, `foldr f z (Tree a) == foldr f z (List a)`, `foldr` over a `Tree` of target type `a` is the same as `foldr` over a `List` of target type `a`. The structure information of `Tree` disappeared.
   - `Tree` and `List` are gone. Only `a*a*...*a` information.
        ``` 
        > t1 = Node 1 []
        > t2 = Node 2 []
        > t3 = Node 3 []
        > t4 = Node 4 []
        > t5 = Node 5 [t1,t2]
        > t6 = Node 6 [t3,t4]
        > t7 = Node 7 [t5,t6]
        > foldr (:) [] t7
            [7,5,1,2,6,3,4]
        > flatten t7
            [7,5,1,2,6,3,4]
        ```
        So We could construct a `List a` from `Tree a` based on `foldr` using `(:)` to replace `*` and use `[]` to terminate aggregate function of type ` [] -> []`.
        ```
        instance Foldable Tree where
            ...
            toList = flatten
            ...
        -- > flatten (Node 1 [Node 2 [], Node 3 []]) == [1,2,3]
        flatten :: Tree a -> [a]
        flatten t = squish t []
            where squish (Node x ts) xs = x:Prelude.foldr squish xs ts
        ```


3. **`foldr`** Generalized Iintuition. 
    - More than one value in `Parametric Type :: t a` **means** t is or wrapping a product type `a*a*...*a`.
    - Being instance of `Foldable` **means** `t` only implies the existence of `a*a...*a`. All other information such as structure information of being `Tree` or `List` are irrelevant.
    - `foldMap` is the basic function that replace `a` with `Monoid m`; replace `*` with `<>`.
    - `foldr` is an extension of `foldMap`. It equivalent to 
      - Treat `t a` of any type as `List t`.
      - Replace `Con` with `f`.
      - Replace `Nil` with `z`.
    - Values of target type `a` got folded.
    - Foldable includes the toList :: Foldable t => t a -> [a] method. That means any Foldable data structure can be turned into a list [[haskell wikibook]](https://en.wikibooks.org/wiki/Haskell/Foldable#List-like_folding) 
 
4. **Summary**\
    For a parametric type `'t a'` being an instance of `Foldable` means we could use `foldMap` or `foldr` to **fold** value(s) of target type `a`. So basically `t a` is `Foldable` when it is an instance of `Foldable`.\
    Pretty self-explanatory.

## Others 
### **1. [foldrM](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldrM)**

|function| constraint|type| define | import |    
|:--:|:--:|:--:|:--:|:--:|:--:|
|`foldrM`| `Monad m, Foldable t`| `(a -> b -> m b) -> b -> t a -> m b`| `Data.Foldable` | `Data.Foldable`|
|`foldlM`| `Monad m, Foldable t`| `(b -> a -> m b) -> b -> t a -> m b`| `Data.Foldable` | `Data.Foldable`|
|`foldM`| `Monad m, Foldable t`| `(b -> a -> m b) -> b -> t a -> m b`| `Control.Monad (=foldlM)` | `Control.Monad`|

**Starts from `foldr`**
1. The semantics of `foldr` is:
    - `t a` is a collection of elements with the same type `a`.
    - a function `f ` of type `a -> b -> b`
    - a single element of type `b`.
    - `foldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b`:  
        > each element of `t a` contribute a piece of information to a element of type `b` through function `f`.
    - `Foldable` treat all structure `t` equally as a `List`.
    - `foldr` replaces component of `List a`. It replaces  `Con` or `:` with `f`, and `[]` with `z`. 

1. The semantics of `foldrM`:

- The type of `foldrM` is :
 `(Monad m, Foldable t) => (a -> b -> m b ) -> b -> t a -> m b`
 It means we have:
     1. a function: `f :: ( a -> b -> m b) `
     2. a List or Tree: ` xs :: t a`
     3. an initial value: `z :: b`
     4. provide all 3 above we get a `r :: m b`

- 1 step further:
  map `f` over `xs`. 
 `f <$> xs` get a list of type : `[(b -> m b), ( b -> m b) ..., (b -> m b)]`

 - 2 step further:
 now the type is :
 ` t (b -> m b) -> ( b -> m b)`
 it is basically turn `[(b -> m b), ( b -> m b) ..., (b -> m b)]` to ` b -> m b`
 **Semantics of foldM**: bind these monadic computation together. 

 - 3 step further: 
 **HOW**
 we have ` (b -> m b) : (b -> m b) : ... : (b -> m b) : []`
> Option A:
> Replace `:` with `=<<` , `[]` with `m b`
> we have `(b -> m b) =<< (b -> m b) =<< ... =<< ( b-> m b) =<< m b`
> In this case: 
>>` foldrM f z xs = foldr (=<<) (pure z) (f <$> xs)`
>
> Option B:
> Replace `:` with `<=<`, `[]` with `b -> m b`
> we have `(b -> m b) <=< (b -> m b) <=< ... <=< ( b-> m b) <=< (b -> m b)`
> In this case: 
> >`foldrM f z xs = foldr (<=<) pure (f <$> xs) $ z`

Example:

>```
>*>import Control.Monad.Trans.Writer    -- Use Wirter Monad in this example.
>*>import Control.Monad                 -- import (<=<)
>*>import Data.Foldable                 -- import foldr, foldrM, etc.
>
>*>:{
>*|wf a b = do 
>*|        tell $ show b             -- introduce String type log
>*|        return $ a * b            -- target computation a*b.
>*|:}
>*>:info wf
>*w :: (Monad m, Show b, Num b) => b -> b -> WriterT String m b
>
>*>let tl = [1,2,3,4]
>*>let z = 1
>
>*>r1 <- runWriterT $ foldrM wf z tl
>*>r1
>*(4,"141224")
>
>*>let r2 = foldr (=<<) (pure z) $ wf <$> tl
>*>rr2 <- runWriterT r2
>*>rr2
>*(4,"141224")
>
>*>let r3 = foldr (<=<) pure (wf <$> tl) $ z
>*>rr3 <- runWriterT r3
>*>rr3
>(24,"141224")
>```

### `foldrM` Summary 
- `foldrM :: (Foldable t, Monad m) => ( a -> b -> m b ) -> b -> t a -> m b`
- `foldrM` transform value of type a in `List a` to a monadic computation ` b -> m b`
    > each element of `t a` contribute a piece of information to the computation ` b -> m b`.
- then, these computataion of type `b -> m b` bind together from right to left.
- With a initial value z of type `b`, we get the result of type `m b`.


## TODO: 
 1. Why is this `flatten` in `Data.Tree` better than `foldr` version above.
 1. How to rewrite foldM(foldlM) in the form of foldr + fmap
    > The official definition of [`foldlM`](https://hackage.haskell.org/package/base-4.7.0.2/docs/src/Data-Foldable.html#foldlM) and [`foldrM`](https://hackage.haskell.org/package/base-4.7.0.2/docs/src/Data-Foldable.html#foldrM) is hard to understand. 
    ```
    *> let pl = foldM wf 1 tl 
    *> l <- runWriterT pl
    *> l
    (24,"1234")
    ```
    Try combine the answer in [stackoverflow](https://stackoverflow.com/questions/58443912/what-kind-of-knowledge-or-training-is-necessary-for-someone-to-write-down-the-de) to get an intuitive understanding.
 1. Intuition about all examples in [youtube ConfEngine](https://www.youtube.com/watch?v=t9pxo7L8mS0)