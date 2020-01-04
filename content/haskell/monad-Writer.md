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
- Target type : `a`
- Context type : `Writer w`

Two folds meaning:
Engineering
We focus on transformation of `target types` **explicitly** and let this monad handle certain `Context information` **implicitly**.

general:
mapping relation and context which affect the mapping relation could be factorized in this form.

 TODO

> - The product of two types `w` and `b` is `(b,w)`.
> - We have a new type `newtype Writer w a = Writer {runWriter :: (a,w)}`
> - In `Writer Monad` we care about computation compositions :
>> - `>>= :: Writer w a -> (a -> Writer w b) -> Writer w b`.
>> - `>=> :: (a -> Writer w b) -> ( b -> Writer w c) -> (a -> Writer w c)`
> - **Explicitly**, we focus on transformation of `target types :: a -> b -> c`.
> - **Implicitly**, `Writer Monad` accumulate extra `logging` information `w` of a series of computations: `a->(b,w)`, `b->(c,w)` and `c->(d,w)`.



- auxiliary functions `tell`,`return`, `listen(s)`, `pass`,`censor`
- Each result in a Writer monad.
- These special purpose Writer Monad are composed by `>>` operator usually.
- They collectively work as a single Writer Monad for certain function.

### auxiliary function

- **wrtier**: `lift` and `wrap` a type product `(a,w)` to be a `WriterT` monad.
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
- Retrieve logging information `w` and update `target type` `a` to `(a,w)`. So that we could process `logging information w` explicitly.
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