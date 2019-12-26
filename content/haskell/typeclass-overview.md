---
title: "Type Overview"
date: 2019-12-24T10:58:12+08:00
draft: true
---

### online source

- [typeclass wiki](https://wiki.haskell.org/Typeclassopedia)
- [optic types](http://oleg.fi/gists/posts/2017-04-18-glassery.html)
- [optic well typed](http://www.well-typed.com/blog/2019/09/announcing-the-optics-library/)

### Note
- Enginerring point of view: haskell could provide better factorization. 
    - `Better` means: easy to matinaine, update or modify.
      - Influence of  Local modification is predictable :: Means if when facing requriment of modification , we only need to modify local. and It won't affect other functioning part. 


Different sematics  :: affect the function of target type . 

**One target type at one time**

|  | List |product    |Sum   |  -> |   
|:--|:--:|:--:|:--:|:--:|
| `<$> :: a -> b -> f a -> f b`| Container | Container |Container |Container |
| `<*> :: f (a -> b) -> f a -> f b`    | Generator|Container |Container |Container |
| `<|>`    | | | | |
| `>>=`| | | | |

- `Container` : Value/s of Type a ( deterministic)
- `Generator` : Value/s of Type a ( non-deterministic)

- Functor : <$>
- Applicative 
- Alternative
- Monad
    - MonadPlus
    - MonadFix

### Group Two
- Foldable: (Monoid m ) => foldMap --> Others
- Traversable


### Group Three
- MonadTrans
- MonadIO
- MonadFail
- Contravarriant
