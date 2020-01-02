---
title: "Monad: Writer"
date: 2019-11-06T11:15:38+08:00
draft: true
---

#### Path
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

{{< image src="/imgs/lyah_writer.png" alt="Writer Monad" position="center" style="border-radius: 8px;" >}}


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
> - A piece of information `s` affects the output of the computation `a`.
>> `s -> a`
> - Sometimes, the value of `s` will be updated and could affect following computation, so need to be preserved.
>>  `s -> (a,s)` 
> - Two or more computations like this composed by `>>=` or `>=>` means each computation use the information `s` passed by previous one.
>> ` f >>= g :: s0 -> (a,s1) -> ( a -> s1 -> (b,s2)) -> (s0 -> (b,s2)`