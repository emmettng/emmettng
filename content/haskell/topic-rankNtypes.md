---
title: "Topic: RankNTypes"
date: 2019-10-30T11:16:02+08:00
draft: true
---

# Note about rankNtypes 

### Online resources: 

- [stackoverflow](https://stackoverflow.com/questions/12031878/what-is-the-purpose-of-rank2types/12033549#12033549)
- [stackoverflow](https://stackoverflow.com/questions/59098273/define-data-type-include-constrained-functions/59098424#59098424)
- [24 days](https://ocharles.org.uk/guest-posts/2014-12-18-rank-n-types.html)


`RankNTypes` really means `Rank N polymorphism`

- There is nothing you could do if there is absolutely no constraint 
  ```
  type IdFunc :: forall a . a -> a

  funcOne :: IdFunc 
  funcOne a = a
  ```
  - Rank 1 : we need to provide funcOne a certain type, so certain type can be given
### Interesting examples