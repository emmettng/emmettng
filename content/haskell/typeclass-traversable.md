---
title: "Typeclass Traversable"
date: 2020-01-09T14:43:58+08:00
draft: true
---

### Reading List 
- [Typeclass: Foldable]()
- [wiki book: Traversable](https://en.wikibooks.org/wiki/Haskell/Traversable)



>  `To traverse means to walk across, and that is exactly what Traversable generalises: walking across a structure, collecting results at each stop.`

`t a` is a collection of values of the same type `a`. `t` contains the 
structure information about how these values is organized.

Three key concepts:
1. `structure`: The got preserved
1. `collecting`: Compute the transformed values in the Applicative context.
1. `each stop`: Above computation is organized as the unit of a list.


- `collection` means `foldr`
```
instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = List.foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys
```

- `each stop` means `List structure` as a collecting unit.
```
instance Traversable Tree where
    traverse f (Node x ts) = liftA2 Node (f x) (traverse (traverse f) ts)
```
The information of being a `List` Structure got keep.


>Traversable is to Applicative contexts what Foldable is to Monoid values. From that point of view, sequenceA is analogous to fold − it creates an applicative summary of the contexts within a structure, and then rebuilds the structure in the new context. sequenceA is the function we were looking for.
If sequenceA is analogous to fold, traverse is analogous to foldMap. They can be defined in terms of each other, and therefore a minimal implementation of Traversable just needs to supply one of them:
```
traverse f = sequenceA . fmap f
sequenceA = traverse id
```

```
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  ...
  	-- Defined in ‘Data.Traversable’
Prelude GHC.Base Control.Monad> :info foldMap
class Foldable (t :: * -> *) where
  ...
  foldMap :: Monoid m => (a -> m) -> t a -> m
  ...
  	-- Defined in ‘Data.Foldable’
```

TODO
- read **wiki book Traversable** and combine with **Typeclass: Foldable**.
- intuition [wiki book interpretation](https://en.wikibooks.org/wiki/Haskell/Traversable#Interpretations_of_Traversable)
- relation between fmap,foldMap,and traverse [wiki book](https://en.wikibooks.org/wiki/Haskell/Traversable#Recovering_fmap_and_foldMap)