---
title: "Typeclass Foldable"
date: 2020-01-06T00:34:46+08:00
draft: true
---
### Reading List
- [Haskell wikibook: Foldable](https://en.wikibooks.org/wiki/Haskell/Foldable)
- [Youtube Conf](https://www.youtube.com/watch?v=t9pxo7L8mS0)

## Parametric Type : `t a`
- Target type : `a`
- Context type : `t`
  
## Intuition for ADT Understanding
> This section needs more theoretical backup.\
> Jump to `Intuition for Real World Implementation` for recap.

| instance declaration| `Context type` | define |
|:--:|:--:|:--:|
|`instance Foldable`|`Tree`|in ‘Data.Tree’|
|`instance Foldable`|`[]`|in ‘Data.Foldable’|
|`instance Foldable`|`Maybe`|in ‘Data.Foldable’|
|`instance Foldable`|`(Either a)`|in ‘Data.Foldable’|
|`instance Foldable`|`((,) a)`|in ‘Data.Foldable’|

### It is all about `foldMap`
```
typeClass Foldable  where 
    ...
    foldMap :: Monoid m => (a -> m) -> t a -> m`
    ...
```
- ***a***. Replace `Target Type` with `Monoidal` type `m`.
- ***b***. Aggregate values of type `m` with `<>`, if there are many values of `m`.

>**1.One value of `Target type`** in the Data Structure
>
>- Because there is only one value of type `m`:
>   - Step ***b*** is not necessary in this case.
>   - Just need to guarantee the `target type` to be `m` and other type to be `mempty` (`Sum type`) or simply being ignored (`Product type`).
> 
>- `Sum Type`: 
>    - [`Maybe`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html):
>       ```
>       instance Foldable Maybe where
>           foldMap = maybe mempty
>           ...
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
>   - Type `List a`: means a data structure contains `1`:`No information of a`, or `a` : one value of type `a`, or two values of type `a`, etc.
>       - `+` means `or`.
>       - `a * a` equivalent to cartesian product of set `a` and `a`.
>   - Type `Tree a`: means always a piece of information of type `a`, or `a` and possible one or more trees `(T a)^n`.
>       - The first expansion of `T a`: `a+a*a+...+a*a*List(T a)` is quite similar with the expansion of `List a` : `1+a+a*a+...`.
> 
> 2. A product `m*m*m` needs to \
>       **Aggregate values of type `m` with `<>`**\
>       **Aggregate values of type `m` with `<>`**\
>       **Aggregate values of type `m` with `<>`**\
>   means replace `*` of `product type` with `<>`.\
>   Now:\
>   `m * m * m`    ==>  `m <> m <> m`.\
>   Assume function `f :: Monoid m => a -> m`, if we take the first expansion of List `L` and Tree `T`, because `+` means `or`, therefore:
>>  - `foldMap f L -> m <> m <> m <> m ...`
>>  - `foldMap f T -> m <> m <> m <> m ...`
>   The computation `foldMap f L` are essentially the same as `foldMap f T`, except:
>   1. When `L` is empty `foldMap f L = mempty`.
>       ```
>       foldMap f = foldr (mappend . f) mempty
>
>       foldr k z = go
>             where
>               go []     = z
>               go (y:ys) = y `k` go ys
>       ```
>   2. `T` cannot be empty, it must contains at least one value of type `a`, it is equivalent to a List of only one element. In this case `foldMap f L = foldMap f T = m` 

### Summary
- `List` and `Tree` as the instance of `Foldable` are equivalent up to the definition of `foldMap` (except for foldMap over empty list). More generally speaking, all `Product Type` are essentially the same when being instance of `Foldable`.
- This make sense, because a `typeclass` reflects only one particular aspect of a given type. In other words, it defines some common property of many types which may contains more information but will be ignored when being treated as a `instance` of this `typeclass`.
 
Theoritically Speaking 
1. List and Tree as the algibraic datatype. 
2. foldMap, foldr and Monoid typeclass
3. How foldr is replace of List and Tree 

List and Tree indicates certain data structure of type `a`
it is Foldable means:
- we could map each element of type `a` in this structure to be a function of type `b -> b`.
- **IMPORTANT PART**: these functions of type `b -> b` could composed together (**FOLD**) to be new function of type ` b -> b`
- Then we feed this new function of type `b -> b` with an input of type `b`, we get the output.
- foldMap and foldr can define each other with above semantics. 

TODO: 
unify the definition of foldMap with respect to List and Tree. 

## Intuition for Real World Implementation
Real word structure 
1. [] as instance of Foldable, foldr instead of foldMap
2. Tree as instance of Foldable, foldMap instead of foldr
3. How these function export and defined.

```
foldComposing :: (a -> (b -> b)) -> [a] -> Endo b
foldComposing f = foldMap (Endo . f)

foldr :: (a -> b -> b) -> b -> t a -> b
foldr f z t = appEndo (foldMap (Endo . f) t) z
```
these `Endo . f` doesn't has to be evaluated in any particular order.

TODO:
- start from the Endo explination [wikibook](https://en.wikibooks.org/wiki/Haskell/Foldable) + list replace intuition [youtube](https://www.youtube.com/watch?v=t9pxo7L8mS0)
- Generalize to the understanding of other data type, such as [Data.Tree](https://hackage.haskell.org/package/containers-0.6.2.1/docs/src/Data.Tree.html#Forest)
- Firgure out what is `[]` and `Data.Tree` is in the position of `ADT`.