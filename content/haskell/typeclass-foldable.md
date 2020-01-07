---
title: "Typeclass Foldable"
date: 2020-01-06T00:34:46+08:00
draft: true
---
### Reading List
- [Haskell wikibook: Foldable](https://en.wikibooks.org/wiki/Haskell/Foldable)
- [Youtube Conf](https://www.youtube.com/watch?v=t9pxo7L8mS0)

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

Real word structure 
1. [] as instance of Foldable, foldr instead of foldMap
2. Tree as instance of Foldable, foldMap instead of foldr
3. How these function export and defined.

```
foldComposing :: (a -> (b -> b)) -> [a] -> Endo b
foldComposing f = foldMap (Endo . f)

foldr :: (a -> b -> b) -> b -> t a -> b
foldr f z t = appEndo (foldMap (Endo #. f) t) z
```
these `Endo . f` doesn't has to be evaluated in any particular order.

TODO:
- start from the Endo explination [wikibook](https://en.wikibooks.org/wiki/Haskell/Foldable) + list replace intuition [youtube](https://www.youtube.com/watch?v=t9pxo7L8mS0)
- Generalize to the understanding of other data type, such as [Data.Tree](https://hackage.haskell.org/package/containers-0.6.2.1/docs/src/Data.Tree.html#Forest)
- Firgure out what is `[]` and `Data.Tree` is in the position of `ADT`.