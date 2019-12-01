---
title: "Topic: Orphan Instance"
date: 2019-11-01T10:56:58+08:00
draft: true
---

## Orphan instance 

#### Online docs:
1. [stackoverflow](https://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell)
2. [wiki haskell](https://wiki.haskell.org/Orphan_instance)
3. [ghc document](https://downloads.haskell.org/~ghc/6.10.3/docs/html/users_guide/separate-compilation.html#orphan-modules)

### 1.What is it ?
- A type class `T` is defined in file `fileT.hs`
- A certain Type `C` is defined in file `fileC.hs`
- `instance T C where ...` is defined in file `fileA.hs`     
- It would be possible that there is another definition : `instance T C where..` in file `fileB.hs`
- These two instance might be imported in future in a same model (`fileC.hs`) and there is no mechanism for haskell to decide which one to use.

### 2. Notes
 If this requirement is necessary the [common work around](https://wiki.haskell.org/Orphan_instance#Common_workaround) is to use **newtype** to provide a **wrapper** that contains the information of this need.


