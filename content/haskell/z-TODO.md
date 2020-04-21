---
title: "Z TODO"
date: 2020-04-21T10:35:11+08:00
draft: true
---

1. Type families 
    1. type-level programming 
1. functional dependencies 

- [online discussion](https://news.ycombinator.com/item?id=14218453)
    What's the difference in motivation between doing things this way vs with functional dependencies?


    Type families are more general than fundeps, and can lead to simpler code with fewer extensions—FunctionalDependencies requires MultiParamTypeClasses, and often FlexibleInstances, FlexibleContexts, and UndecidableInstances.
    Fundeps are pretty clear for some simple examples (e.g., a container’s element type is uniquely determined by the container type), but in general I find it easier to think in terms of type-level functions than dependencies between types.

- [reddit discussion 1](https://www.reddit.com/r/haskell/comments/67qd6w/basic_type_level_programming_in_haskell/)