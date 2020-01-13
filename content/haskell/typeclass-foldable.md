---
title: "Typeclass: Foldable"
date: 2020-01-06T00:34:46+08:00
draft: true
---
### Reading List
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

### It starts from `foldMap`

- One or more value of target type `a` being organized in some **Data Structure**.
- `foldMap`:
  - ***a***. Replace `Target Type` with `Monoidal` type `m`.
  - ***b***. Aggregate values of type `m` with `<>`, if there are many values of `m`.
    ```
    typeClass Foldable  where 
        ...
        foldMap :: Monoid m => (a -> m) -> t a -> m`
        ...
    ```


>**1.One value of `Target type`** in the Data Structure
>
>- Because there is only one value of type `m`:
>   - Step ***b*** is not necessary in this case.
>   - Just guarantee the `target type` to be `m` and other type to be `mempty` ( in `Sum type :: | `) or simply being ignored ( in `Product type :: (,)`).
> 
>- `Sum Type`: 
>    - [`Maybe`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html):
>       ```
>       instance Foldable Maybe where
>           foldMap = maybe mempty
>           ...
>
>       maybe :: b -> (a -> b) -> Maybe a -> b 	-- Defined in ‘Data.Maybe’
>       ```
>    - [`Either a`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html):
>       ```
>       instance Foldable (Either a) where
>           foldMap _ (Left _) = mempty
>           foldMap f (Right y) = f y
>           ...
>       ```
>- `Product Type`:
>    - [`(,) a`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html):
>       ```
>       instance Foldable ((,) a) where
>           foldMap f (_, y) = f y
>           ...
>       ```
>
>**2.More values of `Target type`** in the Data Structure
>
>> **This is the main part of this doc.**
>
> 1. If there is more than one value of the `target type` in the data structure, they must be organized as a `Product type` of the same `Target type`. Two most used examples:
>       - `List`
>           ```
>           List a -> f a  = 1 + a * f a
>                          = 1 + a * (1 + a * (1 + a * (1 + a*(1 + a * ...))))
>                          = 1 + a + a*a + a*a*a + ... + a*a*.... 
>           ```
>       - `Tree`
>           ```
>            List a -> f a = 1 + a * f a
>            Tree a -> T a = a * List (T a)
>                          = a * (1 + T a * f T a)
>                          = a * (1 + a * (1 + T a * f T a))
>                          = a * (1 + a * (1 + a * (1 + T a * f T a)))
>                          = a + a*a + a*a*a + ... + a*a*a*List(T a)
>           ```
>            or replace `a` in `List a` with `T a`, then:
>           ``` 
>            Tree a -> T a = a * List (T a)
>                          = a * (1 + (T a) + (T a)^2 + ... + (T a)^n)
>                          = a + a * (T a) + a * (T a)^2 + ...+ a * (T a)^n
>           ```
> So we know:
>   - Type `List a`: means a data structure contains No information of `a` ( `1`), or one value of type `a` (`a`), or two values of type `a`(`a*a`), etc.
>       - `+` means `or`.
>       - `a * a` equivalent to cartesian product of set `a` and `a`.
>   - Type `Tree a`: means always a piece of information of type `a`, or `a` and possible one or more trees `(T a)^n`.
>       - The first expansion of `T a = a+a*a+...+a*a*List(T a)` is quite similar with the expansion of `List a = 1+a+a*a+...`.
> 
> 2. A product `m*m*m` needs to \
>       1. **Aggregate values of type `m` with `<>`**.  (step ***b***)
>       1. **Aggregate values of type `m` with `<>`**.  (step ***b***)
>       1. **Aggregate values of type `m` with `<>`**.  (step ***b***)\
>   means replace `*` of `product type` with `<>`. \
>   (Is this the only possible option ? With minimal requirement of information and as the instance of Foldable, **YES!**).\
>   Now:\
>   `m * m * m`    ==>  `m <> m <> m`.
> 
> Assume function `f :: Monoid m => a -> m`, if we take the first expansion of List `L` and Tree `T`, we have:
>>  - `foldMap f L -> m <> m <> m <> m ...`
>>  - `foldMap f T -> m <> m <> m <> m ...`
>
>>   The computation `foldMap f L` are essentially the same as `foldMap f T`, except:
>>    1. When `L` could be empty, then: `foldMap f L = mempty`.
>>        ```
>>        foldMap f = foldr (mappend . f) mempty
>>
>>        foldr k z = go
>>              where
>>                go []     = z
>>                go (y:ys) = y `k` go ys
>>        ```
>>    2. `T` cannot be empty, it must contains at least one value of type `a`, it is equivalent to a List of only one element. In this case `foldMap f L = foldMap f T = m` 

### Summary
- `List` and `Tree` as instances of `Foldable` are equivalent up to the definition of `foldMap` (except for foldMap over empty list). More generally speaking, all `Product Type` are essentially the same when being instance of `Foldable`.
- This make sense, because a `typeclass` reflects only one particular aspect of a given type. In other words, it defines some common property of many types which may contains more information but could be ignored when being treated as a `instance` of this `typeclass`.
- What `foldMap` does to a `Parametric Type` that is an instance of `Foldable`:
  1. replace target type `a` with a monoid `m`.
  2. When there are more values of `a`, replace `*` with `<>`. (***Whatever data structure it is***).
  3. When there is only one value of `a`, replace `a` with `m` and others with `memtpy`.
- **foldr**
  ```
  class Foldable (t :: * -> *) where
    ...
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo . f) t) z
    ...
  ```
  1. `appEndo (foldMap (Endo . f) t)` 
      This operation 
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
  4. [`foldr`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldr) and [`foldMap`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldMap) can define each other.
        ```
        foldMap :: Monoid m => (a -> m) -> t a -> m
        foldMap f = foldr (mappend . f) mempty

        foldr :: (a -> b -> b) -> b -> t a -> b
        foldr f z t = appEndo (foldMap (Endo #. f) t) z
        ```
## Intuition for Real World Implementation


1. **`foldr`** [intuition for `List`](https://www.youtube.com/watch?v=t9pxo7L8mS0)
    - Replace `Con` with `f`.
    - Replace `Nil` with `z`.
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
### **1. [foldlM](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldlM)**

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
    - `foldrl :: (Foldable t) => (a -> b -> b) -> b -> t a -> b`:  
        > each element of `t a` contribute a piece of information to a element of type `b` through function `f`.
    - `Foldable` treat all structure `t` equally as a `List`.
    - `foldr` indicates that we retrieve element of `t a` from the right side of the list `t a`.

1. The semantics of `foldrM` is:

>- Assuming we need a function of type:
> `(Monad m, Foldable t) => (a -> b -> m b ) -> b -> t a -> m b`
> Usually it means we have:
> 1. a function: `f :: ( a -> b -> m b) `
> 2. a List or Tree: ` xs :: t a`
> 3. an initial value: `z :: b`

>- One step further:
> Usually, we map `f` on `xs`. 
> `f <$> xs` get a list of functions: `[(b -> m b), ( b -> m b) ...]`

>- Now we have 
>> 1. an initial value: `z :: b`
>> 2. a list functions: `[(b -> m b), ( b -> m b) ...]`
>
>>And we want a value of type ` m b`.
> It's `t ( b -> m b) -> b -> m b`

I think this can be rewritten like this:
> It's `t ( b -> m b) -> (b -> m b)`
```
tb:: t (b -> m b)
foldr (=<<) pure tb :: b -> m b
```
```
tb:: t (b -> m b)
foldr (<=<) pure tb :: b -> m b
```

>- One more step further:\
> The reasonable and minimal information required operation is combine the `initial value` and a `list of function` with `>>=`. Like this: \
> ` m b >>= ( b-> m b) >>= ( b -> m b) ... `

> NOW
>> 1. we have a list of functions of type `[f1, f2,..., fn ] :: [(b ->m b), (b -m b), ...]`.
>> 1. we know it combine like this ` m b >>= (b -> m b) >>= (b -> m b)`. But:
>>  - `mb >>= f1 >>= f2 >>= ...  >>= fn`
>>  - `mb >>= fn >>= ... >>= f2 >>= f1`
> - WHICH?

> fold from right `mb >>= fn >>= ... >>= f2 >>= f1`
> fold from left `mb >>= f1 >>= f2 >>= ...  >>= fn`

> This is about `foldr`
> - replace `a` with `f`
> - replace `<>` with `.`
> - `<=<` keeps the composition order.
```
> let c = (return <=<) . (return <=<)
> :info c
c :: Monad m => (a -> m c) -> a -> m c
  	-- Defined at <interactive>:11:5
> let c = (return >=>) . (return >=>)
> :info c
c :: Monad m => (b -> m c) -> b -> m c
  	-- Defined at <interactive>:13:5
> :info (<=<)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
  	-- Defined in ‘Control.Monad’
infixr 1 <=<
> :info (>=>)
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
  	-- Defined in ‘Control.Monad’

Prelude GHC.Base Control.Monad> :info (.)
(.) :: (b -> c) -> (a -> b) -> a -> c 	-- Defined in ‘GHC.Base’
infixr 9 .
Prelude GHC.Base Control.Monad> :info (<=<)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
  	-- Defined in ‘Control.Monad’
infixr 1 <=<

```
[stackoverflow question](https://stackoverflow.com/questions/58443912/what-kind-of-knowledge-or-training-is-necessary-for-someone-to-write-down-the-de)
```
(b -> m b) -> (b -> m b) -> (b -> m b) -> ( b -> m b) -> t ( b -> m b) - (b -> m b) 
= foldr <=< return (f <$> xs) 
= foldMap <=< (f<$>xs) $ return
```
> TODO: all information above, needs more intuitive organization.


## TODO: 
 1. Why is this `flatten` in `Data.Tree` better than `foldr` version above.
 2. Intuition about all examples in [youtube ConfEngine](https://www.youtube.com/watch?v=t9pxo7L8mS0)

foldr + fmap vs foldrM
```
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> :{
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable| let wf a b = do 
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable|             tell $ show b
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable|             pure $ a*b
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable| :}
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> let tl = [1,2,3,4]
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> let tm = wf <$> tl
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> :info wf
wf :: (Monad m, Show b, Num b) => b -> b -> WriterT String m b
  	-- Defined at <interactive>:158:5
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> :info tm
tm :: (Monad m, Show b, Num b) => [b -> WriterT String m b]
  	-- Defined at <interactive>:163:5
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> let p1 f = foldr (=<<) f tm
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> :info p1
p1 ::
  (Monad m, Show a, Num a) =>
  WriterT String m a -> WriterT String m a
  	-- Defined at <interactive>:166:5
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> let r1 = p1 $ writer (1,"")
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> rr1 <- runWriterT r1
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> rr1
(24,"141224")
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> let p2 = foldrM wf 1 tl
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> :info p2
p2 :: (Monad m, Show b, Num b) => WriterT String m b
  	-- Defined at <interactive>:171:5
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> rr2 <- runWriterT p2
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> rr2
(24,"141224")
```


How to rewite foldM in the form of foldr + fmap
```
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> let pl = foldM wf 1 tl 
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> l <- runWriterT pl
*Main Control.Monad Control.Monad.Trans.Writer Data.Foldable> l
(24,"1234")
```